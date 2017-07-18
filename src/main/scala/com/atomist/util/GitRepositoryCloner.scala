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
            depth: Int = Depth): File = {
    val repoStr = s"$owner/$repo"
    try {
      val br = branch.map(b => if (b == "master") "" else s" -b $b").getOrElse("")
      val repoDir = createRepoDirectory(repo, owner, dir)
      val cloneCmd = s"git clone$br --depth $depth --single-branch $getUrl/$repoStr.git ${repoDir.getPath}"
      logger.debug(s"Executing command $cloneCmd")
      cloneCmd !! outLogger
      sha.map(resetToSha(_, repoDir, repoStr))
      repoDir
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Failed to clone $repoStr", e)
    }
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

  private def resetToSha(sha: String, repoDir: File, repoStr: String) = {
    logger.warn(s"Commit sha $sha specified so attempting to reset")
    val resetProcess = Process(s"git reset --hard $sha", repoDir)

    val rc = resetProcess ! outLogger
    if (rc != 0) {
      logger.warn(s"Failed to reset to sha $sha. Attempting to fetch entire repo")
      Process("git remote set-branches origin '*'", repoDir) #&&
        Process("git fetch --unshallow", repoDir) !! outLogger
      logger.debug(s"Successfully fetched entire repo. Attempting to reset to sha $sha")
      val rc2 = resetProcess ! outLogger
      if (rc2 != 0) {
        logger.warn(s"Failed to reset to sha $sha. Attempting to fetch repo without --unshallow")
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