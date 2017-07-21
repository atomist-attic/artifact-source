package com.atomist.source.git

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files}

import com.atomist.source.git.github.GitHubServices.Url
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils

import scala.sys.process._

case class GitRepositoryCloner(oAuthToken: String = "", remoteUrl: Option[String] = None)
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
      val repoDir = createRepoDirectory(repo, owner, dir)
      val br = branch.collect {
        case b if b != "master" => s" -b $b"
      }.getOrElse("")

      s"git clone$br --depth $depth --single-branch $getUrl/$owner/$repo.git ${repoDir.getPath}" !! outLogger
      sha.map(resetToSha(_, repoDir))
      Right(repoDir)
    } catch {
      case e: Exception => Left(e)
    }

  def deleteRepoDirectory(dir: File): Unit = FileUtils.deleteQuietly(dir)

  def cleanRepoDirectory(dir: File): Unit = FileUtils.cleanDirectory(dir)

  private def createRepoDirectory(repo: String, owner: String, dir: Option[File]): File =
    dir.map(file =>
      try {
        Files.createDirectory(file.toPath).toFile
      } catch {
        case e: FileAlreadyExistsException =>
          logger.warn(s"Directory ${file.getPath} already exists, deleting directory content: ${e.getMessage}")
          cleanRepoDirectory(file)
          file
      }).getOrElse(Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}").toFile)

  private def resetToSha(sha: String, repoDir: File) = {
    val resetProcess = Process(s"git reset --hard $sha", repoDir)
    val rc = resetProcess ! outLogger
    if (rc != 0) {
      logger.warn(s"Failed to reset HEAD to specified sha $sha. Attempting to fetch entire repo")
      Process("git remote set-branches origin '*'", repoDir) #&& Process("git fetch --unshallow", repoDir) !! outLogger
      val rc2 = resetProcess ! outLogger
      if (rc2 != 0) {
        logger.warn(s"Failed to reset HEAD to specified sha $sha. Attempting to fetch repo without --unshallow")
        Process("git fetch", repoDir) #&& resetProcess !! outLogger
      }
    }
  }

  private def getUrl = {
    val url = remoteUrl.map(new URL(_)).getOrElse(new URL(Url))
    Option(oAuthToken).collect {
      case token if token != "" => s"${url.getProtocol}://$token@${url.getAuthority}"
    }.getOrElse(url.toExternalForm)
  }
}

object GitRepositoryCloner {

  private val Depth: Int = 10
}