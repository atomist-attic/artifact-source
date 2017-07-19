package com.atomist.util

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils

import scala.sys.process._

case class GitRepositoryCloner(oAuthToken: String = "", remoteUrl: String = GitHubHome.Url)
  extends LazyLogging {

  import GitRepositoryCloner._

  private val outLogger = ProcessLogger(out => logger.info(out), err => logger.warn(err))

  def clone(repo: String,
            owner: String,
            branch: Option[String] = None,
            sha: Option[String] = None,
            dir: Option[File] = None,
            depth: Int = Depth): Either[Throwable, File] =
    try {
      val br = branch.map(b => if (b == "master") "" else s" -b $b").getOrElse("")
      val repoDir = createRepoDirectory(repo, owner, dir)
      val cloneCmd = s"git clone$br --depth $depth --single-branch $getUrl/$owner/$repo.git ${repoDir.getPath}"
      logger.debug(s"Executing command $cloneCmd")
      cloneCmd !! outLogger
      sha.map(resetToSha(_, repoDir))
      Right(repoDir)
    } catch {
      case e: Exception => Left(e)
    }

  def cleanUp(dir: File): Unit = FileUtils.deleteQuietly(dir)

  def resetDirectoryContent(dir: File): Unit = FileUtils.cleanDirectory(dir)

  private def createRepoDirectory(repo: String, owner: String, dir: Option[File]): File =
    dir.map(file =>
      try {
        Files.createDirectory(file.toPath).toFile
      } catch {
        case e: FileAlreadyExistsException =>
          logger.warn(s"Directory ${file.getPath} already exists, resetting directory content: ${e.getMessage}")
          resetDirectoryContent(file)
          file
      }).getOrElse(Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}").toFile)

  private def resetToSha(sha: String, repoDir: File) = {
    val resetProcess = Process(s"git reset --hard $sha", repoDir)
    val rc = resetProcess ! outLogger
    if (rc == 0)
      logger.info(s"Successfully reset HEAD to $sha")
    else {
      logger.warn(s"Failed to reset HEAD to $sha. Attempting to fetch entire repo")
      Process("git remote set-branches origin '*'", repoDir) #&& Process("git fetch --unshallow", repoDir) !! outLogger
      logger.info(s"Successfully fetched entire repo. Attempting to reset HEAD to $sha")
      val rc2 = resetProcess ! outLogger
      if (rc2 != 0) {
        logger.warn(s"Failed to reset HEAD to $sha. Attempting to fetch repo without --unshallow")
        Process("git fetch", repoDir) #&& resetProcess !! outLogger
      }
    }
  }

  private def getUrl = {
    val url = if (Option(remoteUrl).exists(_.trim.nonEmpty)) new URL(remoteUrl) else new URL(GitHubHome.Url)
    if (Option(oAuthToken).exists(_.trim.nonEmpty))
      s"${url.getProtocol}://$oAuthToken@${url.getAuthority}"
    else
      url.toExternalForm
  }
}

object GitRepositoryCloner {

  private val Depth: Int = 10
}