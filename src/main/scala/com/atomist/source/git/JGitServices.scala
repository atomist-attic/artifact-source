package com.atomist.source.git

import java.io.File
import java.nio.file._
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}

import com.atomist.source._
import com.atomist.util.FilePermissions.fromMode
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import org.eclipse.jgit.transport.{RefSpec, UsernamePasswordCredentialsProvider}
import resource._

import scala.collection.JavaConverters._

case class JGitServices(oAuthToken: String, remoteUrl: Option[String] = None)
  extends LazyLogging {

  private val credentialsProvider = new UsernamePasswordCredentialsProvider(oAuthToken, "")
  private val grc = GitRepositoryCloner(oAuthToken, remoteUrl)

  @throws[ArtifactSourceException]
  def createBranchFromChanges(repo: String,
                              owner: String,
                              branchName: String,
                              old: ArtifactSource,
                              current: ArtifactSource,
                              message: String): File = {
    val repoDir = grc.clone(repo, owner) match {
      case Some(dir) => dir
      case None => throw ArtifactSourceException(s"Failed to clone $owner/$repo")
    }

    for (git <- managed(Git.open(repoDir))) {
      val branchRef = s"refs/heads/$branchName"

      val remoteRefs = git.lsRemote.setHeads(true).setCredentialsProvider(credentialsProvider).call
      val remoteBranchExists = remoteRefs.asScala.map(_.getName).exists(_ == branchRef)

      val localBranches = git.branchList.setListMode(ListMode.ALL).call
      val localBranchExists = localBranches.asScala.exists(_.getName == branchRef)

      if (remoteBranchExists && !localBranchExists) {
        git.fetch.setCredentialsProvider(credentialsProvider).setRefSpecs(new RefSpec(s"$branchRef:$branchRef")).call
        git.checkout.setName(branchName).call
      } else
        git.checkout.setCreateBranch(!localBranchExists).setName(branchName).setForce(true).call

      val deltas = current deltaFrom old
      val filesToDelete = processDeltas(repoDir, deltas)
      filesToDelete.foreach(git.rm.addFilepattern(_).call)

      git.add.addFilepattern(".").setUpdate(true).call
      git.add.addFilepattern(".").call

      git.commit.setMessage(message).call
      git.push.setCredentialsProvider(credentialsProvider).call
    }

    repoDir
  }

  private def processDeltas(repoDir: File, deltas: Deltas): Seq[String] = {
    val filesToUpdate = deltas.deltas.collect {
      case fud: FileUpdateDelta => fud.updatedFile
    }
    filesToUpdate.foreach(updateFile(repoDir, _))

    deltas.deltas.collect {
      case fad: FileAdditionDelta if !filesToUpdate.exists(_.path == fad.path) => fad.newFile
    }.foreach(createFile(repoDir, _))

    deltas.deltas.collect {
      case fdd: FileDeletionDelta => fdd.oldFile
    }.map(_.path)
  }

  private def createFile(repoDir: File, fa: FileArtifact): Unit = {
    val filePath = Paths.get(repoDir.getPath, fa.path)
    val perms = fromMode(fa.mode)
    try {
      val fileAttributes = PosixFilePermissions.asFileAttribute(perms)
      Files.createFile(filePath, fileAttributes)
      FileUtils.copyInputStreamToFile(fa.inputStream(), filePath.toFile)
    } catch {
      case _: UnsupportedOperationException =>
        // In case of Windows
        val f = Files.createFile(filePath).toFile
        f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
        f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
    }
  }

  private def updateFile(repoDir: File, fa: FileArtifact): Unit = {
    val filePath = Paths.get(repoDir.getPath, fa.path)
    FileUtils.copyInputStreamToFile(fa.inputStream(), filePath.toFile)
    val perms = fromMode(fa.mode)
    try {
      Files.setPosixFilePermissions(filePath, perms)
    } catch {
      case _: UnsupportedOperationException =>
        // In case of Windows
        val f = filePath.toFile
        f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
        f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
    }
  }
}