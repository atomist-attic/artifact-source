package com.atomist.util

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.FileUtils

import scala.sys.process._

object GitHubHome {

  val Url = "https://github.com"
}

case class GitRepositoryCloner(oAuthToken: String = "", remoteUrl: String = GitHubHome.Url)
  extends LazyLogging {

  import GitRepositoryCloner._

  private val outLogger = ProcessLogger(out => logger.debug(out), err => logger.debug(err))

  def clone(repo: String,
            owner: String,
            branch: Option[String] = None,
            sha: Option[String] = None,
            dir: Option[File] = None,
            depth: Int = Depth): Option[File] = {
    val repoStr = s"$owner/$repo"
    try {
      val br = branch.map(b => if (b == "master") "" else s"-b $b").getOrElse("")
      val repoDir = createRepoDirectory(repo, owner, dir)
      s"git clone $br --depth $depth --single-branch $getUrl/$repoStr.git ${repoDir.getPath}" !! outLogger
      sha.map(resetToSha(_, repoDir, repoStr))
      Some(repoDir)
    } catch {
      case e: Exception =>
        logger.warn(s"Failed to clone '$repoStr'", e)
        None
    }
  }

  def cleanUp(dir: File): Unit = FileUtils.deleteQuietly(dir)

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

  private def resetToSha(sha: String, repoDir: File, repoStr: String) = {
    val resetProcess = Process(s"git reset --hard $sha", repoDir)
    resetProcess #||
      Process("git config remote.origin.fetch +refs/heads/*:refs/remotes/origin/*", repoDir) #&&
      Process("git fetch --unshallow", repoDir) ###
      resetProcess #||
      Process("git fetch", repoDir) ###
      resetProcess !! outLogger
  }

  private def getUrl = {
    val url =  if (Option(remoteUrl).exists(_.trim.nonEmpty)) new URL(remoteUrl) else new URL(GitHubHome.Url)
    if (Option(oAuthToken).exists(_.trim.nonEmpty))
      s"${url.getProtocol}://$oAuthToken@${url.getAuthority}"
    else
      url.toExternalForm
  }
}

object GitRepositoryCloner {

  private val Depth: Int = 10
}