package com.atomist.source.git

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files}

import com.atomist.source.ArtifactSourceCreationException
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier, NamedFileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils

import scala.sys.process._
import scala.util.{Failure, Success, Try}

case class GitRepositoryCloner(oAuthToken: String, remoteUrl: Option[String] = None)
  extends LazyLogging {

  import GitRepositoryCloner._

  private val outLogger = ProcessLogger(out => logger.debug(out), err => logger.debug(err))

  @throws[ArtifactSourceCreationException]
  def clone(repo: String,
            owner: String,
            branch: Option[String] = None,
            sha: Option[String] = None,
            dir: Option[File] = None,
            depth: Int = Depth): FileSystemArtifactSource = {
    val repoStr = s"$owner/$repo"
    val repoDir = Try(createRepoDirectory(repo, owner, dir)) match {
      case Success(file) => file
      case Failure(e) =>
        throw ArtifactSourceCreationException(s"Failed to create target directory for $repoStr", e)
    }

    val br = branch.map(b => if (b == "master") "" else s"-b $b").getOrElse("")
    Try(
      s"git clone $br --depth $depth --single-branch $getUrl/$repoStr.git ${repoDir.getPath}" !! outLogger
    ) match {
      case Success(_) =>
        sha.foreach(resetToSha(_, repoDir, repoStr))
        val fid = NamedFileSystemArtifactSourceIdentifier(repo, repoDir)
        FileSystemArtifactSource(fid, GitDirFilter(repoDir.getPath))
      case Failure(e) => throw ArtifactSourceCreationException(s"Failed to clone $repoStr", e)
    }
  }

  def cleanUp(dir: File): Unit = FileUtils.deleteQuietly(dir)

  def cleanUp(fid: FileSystemArtifactSourceIdentifier): Unit = cleanUp(fid.rootFile)

  def resetDirectoryContent(dir: File): Unit = FileUtils.cleanDirectory(dir)

  def resetDirectoryContent(fid: FileSystemArtifactSourceIdentifier): Unit = resetDirectoryContent(fid.rootFile)

  private def createRepoDirectory(repo: String, owner: String, dir: Option[File]): File =
    dir match {
      case Some(file) =>
        try {
          Files.createDirectory(file.toPath).toFile
        } catch {
          case _: FileAlreadyExistsException =>
            resetDirectoryContent(file)
            file
        }
      case None =>
        val tempDir = Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}").toFile
        tempDir.deleteOnExit()
        tempDir
    }

  private def resetToSha(sha: String, repoDir: File, repoStr: String) =
    Try(Process(s"git reset --hard $sha", repoDir) #||
      Process(s"git config remote.origin.fetch +refs/heads/*:refs/remotes/origin/*", repoDir) #&&
      Process("git fetch --unshallow", repoDir) #&&
      Process(s"git reset --hard $sha", repoDir) !! outLogger) match {
      case Success(_) =>
      case Failure(e) =>
        throw ArtifactSourceCreationException(s"Failed to find commit with sha $sha in $repoStr", e)
    }

  private def getUrl = {
    val url = Try {
      remoteUrl match {
        case Some(gitUrl) => new URL(gitUrl)
        case None => new URL("https://github.com")
      }
    } match {
      case Success(parsedUrl) => parsedUrl
      case Failure(e) => throw ArtifactSourceCreationException(s"Failed to parse remote URL $remoteUrl", e)
    }
    oAuthToken match {
      case token if oAuthToken != null && oAuthToken != "" => s"${url.getProtocol}://$token@${url.getAuthority}"
      case _ => url.toExternalForm
    }
  }
}

object GitRepositoryCloner {

  private val Depth: Int = 10
}
