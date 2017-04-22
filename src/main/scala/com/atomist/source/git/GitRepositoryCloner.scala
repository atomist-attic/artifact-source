package com.atomist.source.git

import java.io.File
import java.net.URL
import java.nio.file.Files

import com.atomist.source.ArtifactSourceCreationException
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier, SimpleFileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class GitRepositoryCloner(oAuthToken: String, remoteUrl: String = "", dir: Option[File] = None) {

  import GitRepositoryCloner._

  @throws[ArtifactSourceCreationException]
  def clone(repo: String, owner: String): FileSystemArtifactSource =
    clone(repo, owner, "master")

  @throws[ArtifactSourceCreationException]
  def clone(repo: String, owner: String, branch: String): FileSystemArtifactSource =
    clone(repo, owner, branch, "")

  @throws[ArtifactSourceCreationException]
  def clone(repo: String, owner: String, branch: String, sha: String): FileSystemArtifactSource =
    clone(repo, owner, branch, sha, Depth)

  @throws[ArtifactSourceCreationException]
  def clone(repo: String, owner: String, branch: String, sha: String, depth: Int): FileSystemArtifactSource =
    try {
      val repoDir = createRepoDirectory(repo, owner)
      val commands = getCloneCommands(repo, owner, branch, depth, repoDir.toString)
      val rc = new ProcessBuilder(commands.asJava).start.waitFor
      rc match {
        case 0 =>
          if (sha != null && sha != "") {
            val rc2 = new ProcessBuilder("git", "reset", "--hard", sha).directory(repoDir.toFile).start.waitFor
            rc2 match {
              case 0 =>
              case _ => throw ArtifactSourceCreationException(s"Failed to find commit with sha $sha. Return code $rc2")
            }
          }
          val fid = SimpleFileSystemArtifactSourceIdentifier(repoDir.toFile)
          FileSystemArtifactSource(fid, GitDirFilter(repoDir.toString))
        case _ => throw ArtifactSourceCreationException(s"Failed to clone '$owner/$repo'. Return code $rc")
      }
    } catch {
      case e: Exception =>
        throw ArtifactSourceCreationException(s"Failed to clone '$owner/$repo'", e)
    }

  private def createRepoDirectory(repo: String, owner: String) =
    dir match {
      case Some(file) =>
        Files.createDirectory(file.toPath)
      case None =>
        val tempDir = Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}")
        tempDir.toFile.deleteOnExit()
        tempDir
    }

  def cleanUp(file: File): Unit = FileUtils.deleteQuietly(file)

  def cleanUp(fid: FileSystemArtifactSourceIdentifier): Unit = cleanUp(fid.rootFile)

  private def getCloneCommands(repo: String, owner: String, branch: String, depth: Int, path: String): Seq[String] = {
    val branchSeq = branch match {
      case br if branch != null && branch != "" && branch != "master" => Seq("-b", br)
      case _ => Seq.empty
    }
    Seq("git", "clone") ++ branchSeq ++ Seq("--depth", depth + "", s"$getUrl/$owner/$repo.git", path)
  }

  private def getUrl = {
    val url = Try {
      if (remoteUrl == null || remoteUrl.isEmpty) new URL("https://github.com") else new URL(remoteUrl)
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
