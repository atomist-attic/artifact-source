package com.atomist.source.git

import java.io.File
import java.nio.file._
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.util.{Set => JSet}

import com.atomist.source._
import com.atomist.util.FilePermissions.fromMode
import com.atomist.util.{GitHubHome, GitRepositoryCloner}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import org.eclipse.jgit.api.{CreateBranchCommand, Git}
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider

import scala.collection.JavaConverters._
import scala.sys.process._
import scala.util.{Failure, Success, Try}

case class GitServices(oAuthToken: String, remoteUrl: String = GitHubHome.Url)
  extends LazyLogging {

  private val outLogger = ProcessLogger(out => logger.info(out), err => logger.warn(err))

  private lazy val grc = GitRepositoryCloner(oAuthToken, remoteUrl)

  //  private val gitHubHeaders: Map[String, String] =
  //    Map("Authorization" -> ("token " + oAuthToken), "Accept" -> "application/vnd.github.v3+json, application/vnd.github.loki-preview+json")

  private val credentialsProvider = new UsernamePasswordCredentialsProvider(oAuthToken, "")

  def createBranchFromChanges(repo: String,
                              owner: String,
                              branchName: String,
                              fromBranch: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): Unit = {
    grc.clone(repo, owner) match {
      case Some(gitDir) =>
        val deltas = current deltaFrom old
        val filesToUpdate = deltas.deltas.collect {
          case fud: FileUpdateDelta => fud.updatedFile
        }
        filesToUpdate.foreach(f => {
          val path = Paths.get(gitDir.getPath, f.path)
          FileUtils.copyInputStreamToFile(f.inputStream(), path.toFile)
          updateFilePermissions(path, f.mode)
        })
        //   logger.info(s"Updated files ${filesToUpdate.map(_.path).mkString(",")}")

        deltas.deltas.collect {
          case fad: FileAdditionDelta if !filesToUpdate.exists(_.path == fad.path) => fad.newFile
        }.foreach(f => {
          val newFile = createFile(Paths.get(gitDir.getPath, f.path), f.mode)
          FileUtils.copyInputStreamToFile(f.inputStream(), newFile)
        })
        //  logger.info(s"Added files ${filesToAdd.map(_.path).mkString(",")}")
        //   val files = filesToUpdate ++ filesToAdd

        val filesToDelete = deltas.deltas.collect {
          case fdd: FileDeletionDelta => fdd.oldFile
        }
        filesToDelete.foreach(f => FileUtils.deleteQuietly(Paths.get(gitDir.getPath, f.path).toFile))
        //   logger.info(s"Deleted files ${filesToDelete.map(_.path).mkString(",")}")

        val start = System.currentTimeMillis
        val git = Git.open(gitDir)
//       val config = git.getRepository().getConfig()
//        val pushBranch = "HEAD:" + pushBranchPrefix + "/" + remoteBranch
//        config.setString("remote", "origin", "push", pushBranch)
//
//        config.setString("branch", remoteBranch, "remote", "origin");
//        String branch = "refs/heads/" + remoteBranch;
//        config.setString("branch", remoteBranch, "merge", branch);
//        config.save();


        val remoteBranchExists = git.lsRemote
          .setCredentialsProvider(credentialsProvider)
          .call
          .asScala
          .map(ref => ref.getName)
          .exists(_ == s"refs/heads/$branchName")
        //   git.fetch.setCredentialsProvider(new UsernamePasswordCredentialsProvider(oAuthToken, "")).call
        val localBranchExists = git.branchList.setListMode(ListMode.ALL).call.asScala.exists(ref => {
          logger.info("####### " + ref.getName)
          ref.getName == s"refs/heads/$branchName"
        })
        logger.info("**** local branch exists: " + localBranchExists)
        //  val remoteBranchExists = getBranch(repo, owner, branchName)
        logger.info("**** remote branch exists: " + remoteBranchExists)
        // git.branchDelete.setBranchNames(refs.map(ref => ref.getName).mkString(",")).setForce(true).call

        //   git.pull.setCredentialsProvider(credentialsProvider).call
  //      git.branchCreate()
//          .setName(branchName)
//          .setUpstreamMode(CreateBranchCommand.SetupUpstreamMode.TRACK)
//          .setForce(!localBranchExists)
//          .call

        git.checkout()
          .setCreateBranch(!localBranchExists)
          .setName(branchName)
          .setUpstreamMode(CreateBranchCommand.SetupUpstreamMode.TRACK)
         // .setStartPoint(s"origin/$branchName")
          // .setStartPoint(s"refs/heads/$branchName")
          .call
        git.add.addFilepattern(".").call
        git.add.setUpdate(true).addFilepattern(".").call
        filesToDelete.foreach(f => git.rm.addFilepattern(f.path).call)
        git.commit.setMessage(message).call
        val p = git.push
          .setCredentialsProvider(credentialsProvider)
          .call
        println("##### pushed ")
        logger.info(s"Time to commit and push updates = ${System.currentTimeMillis - start} ms")
      case None =>
        println("##### none ")
    }
  }

  def clone(repo: String, owner: String, dir: Option[File] = None) = {
    val url = s"$remoteUrl/$owner/$repo.git"
    val repoDir = createRepoDirectory(repo, owner, dir)
    Git.cloneRepository()
      .setURI(url)
      .setCredentialsProvider(credentialsProvider)
      .setDirectory(repoDir)
      .call
      .getRepository
  }

  def cloneCmd(repo: String, owner: String, dir: Option[File] = None) =
    grc.clone(repo, owner, None, None, dir)

  def resetDirectoryContent(dir: File): Unit = FileUtils.cleanDirectory(dir)

  private def createRepoDirectory(repo: String, owner: String, dir: Option[File]): File =
    dir.map(file => {
      try {
        Files.createDirectory(file.toPath).toFile
      } catch {
        case _: FileAlreadyExistsException =>
          resetDirectoryContent(file)
          file
      }
    }).getOrElse(Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}").toFile)

  private def createFile(newPath: Path, mode: Int) = {
    val perms = fromMode(mode)
    Try {
      val fileAttributes = PosixFilePermissions.asFileAttribute(perms)
      Files.createFile(newPath, fileAttributes)
    } match {
      case Success(path) => path.toFile
      case Failure(_: UnsupportedOperationException) =>
        // In case of Windows
        val f = Files.createFile(newPath).toFile
        f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
        f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
        f
      case Failure(t: Throwable) =>
        throw t
    }
  }

  private def updateFilePermissions(path: Path, mode: Int) = {
    val perms = fromMode(mode)
    Try(Files.setPosixFilePermissions(path, perms)) match {
      case Success(path) => path.toFile
      case Failure(_: UnsupportedOperationException) =>
        // In case of Windows
        val f = path.toFile
        f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
        f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
        f
      case Failure(t: Throwable) =>
        throw t
    }
  }

  //  def getBranch(repo: String, owner: String, branch: String): Boolean =
  //    Http(s"https://api.github.com/repos/$owner/$repo/branches/$branch")
  //      .headers(gitHubHeaders)
  //      .asString
  //      .is2xx
}

object GitServices {

  private case class Ref(ref: String, url: String, `object`: RefObject)

  private case class RefObject(`type`: String, sha: String, url: String)
}


