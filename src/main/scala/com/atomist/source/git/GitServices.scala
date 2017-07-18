package com.atomist.source.git

import java.io.File
import java.nio.file._
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}

import com.atomist.source._
import com.atomist.util.FilePermissions.fromMode
import com.atomist.util.{GitHubHome, GitRepositoryCloner}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import org.eclipse.jgit.transport.{RefSpec, UsernamePasswordCredentialsProvider}

import scala.collection.JavaConverters._
import scala.sys.process._
import scala.util.{Failure, Success, Try}

case class GitServices(oAuthToken: String, remoteUrl: String = GitHubHome.Url)
  extends LazyLogging {

  private val outLogger = ProcessLogger(out => logger.info(out), err => logger.warn(err))
  private val credentialsProvider = new UsernamePasswordCredentialsProvider(oAuthToken, "")
  private lazy val grc = GitRepositoryCloner(oAuthToken, remoteUrl)

  def createBranchFromChanges(repo: String,
                              owner: String,
                              branchName: String,
                              fromBranch: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): Unit = {
    grc.clone(repo, owner) match {
      case Some(gitDir) =>
        val start = System.currentTimeMillis
        val git = Git.open(gitDir)

        val branchRef = s"refs/heads/$branchName"
        val remoteBranchExists = git.lsRemote
          .setHeads(true)
          .setCredentialsProvider(credentialsProvider)
          .call
          .asScala
          .map(_.getName)
          .exists(_ == branchRef)

        val localBranchExists = git.branchList
          .setListMode(ListMode.ALL)
          .call
          .asScala
          .exists(_.getName == branchRef)

        logger.info("**** local branch exists: " + localBranchExists)
        logger.info("**** remote branch exists: " + remoteBranchExists)

        if (remoteBranchExists && !localBranchExists) {
          git.fetch.setCredentialsProvider(credentialsProvider).setRefSpecs(new RefSpec(s"$branchRef:$branchRef")).call
          // Process(s"git fetch --depth 1 origin $branchName:$branchName", gitDir) !! outLogger
          git.checkout.setName(branchName).call
        } else
          git.checkout.setCreateBranch(!localBranchExists).setName(branchName).setForce(true).call

        val deltas = current deltaFrom old
        val filesToDelete = updateFiles(gitDir, deltas)
        git.add.addFilepattern(".").call
        git.add.setUpdate(true).addFilepattern(".").call
        filesToDelete.foreach(f => git.rm.addFilepattern(f.path).call)

        git.commit.setMessage(message).call
        git.push.setCredentialsProvider(credentialsProvider).call
        logger.info(s"Time to commit and push updates = ${System.currentTimeMillis - start} ms")
      case None =>
        println("##### none ")
    }
  }

  private def updateFiles(gitDir: File, deltas: Deltas) = {
    val filesToUpdate = deltas.deltas.collect {
      case fud: FileUpdateDelta => fud.updatedFile
    }
    filesToUpdate.foreach(f => {
      val path = Paths.get(gitDir.getPath, f.path)
      FileUtils.copyInputStreamToFile(f.inputStream(), path.toFile)
      updateFilePermissions(path, f.mode)
    })

    deltas.deltas.collect {
      case fad: FileAdditionDelta if !filesToUpdate.exists(_.path == fad.path) => fad.newFile
    }.foreach(f => {
      val newFile = createFile(Paths.get(gitDir.getPath, f.path), f.mode)
      FileUtils.copyInputStreamToFile(f.inputStream(), newFile)
    })

    val filesToDelete = deltas.deltas.collect {
      case fdd: FileDeletionDelta => fdd.oldFile
    }
    filesToDelete.foreach(f => FileUtils.deleteQuietly(Paths.get(gitDir.getPath, f.path).toFile))
    filesToDelete
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
}


