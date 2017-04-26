package com.atomist.source.git

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files}

import com.atomist.source.ArtifactSourceCreationException
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier, NamedFileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class GitRepositoryCloner(oAuthToken: String, remoteUrl: Option[String] = None) {

  import GitRepositoryCloner._

  @throws[ArtifactSourceCreationException]
  def clone(repo: String,
            owner: String,
            branch: Option[String] = None,
            sha: Option[String] = None,
            dir: Option[File] = None,
            depth: Int = Depth): FileSystemArtifactSource =
    try {
      val repoDir = createRepoDirectory(repo, owner, dir)
      val commands = getCloneCommands(repo, owner, branch, depth, repoDir.getPath)
      val rc = new ProcessBuilder(commands.asJava).start.waitFor
      rc match {
        case 0 =>
          sha match {
            case Some(commitSha) =>
              val rc2 = new ProcessBuilder("git", "reset", "--hard", commitSha).directory(repoDir).start.waitFor
              rc2 match {
                case 0 =>
                case _ => throw ArtifactSourceCreationException(s"Failed to find commit with sha $commitSha. Return code $rc2")
              }
            case None =>
          }
          val fid = NamedFileSystemArtifactSourceIdentifier(repo, repoDir)
          FileSystemArtifactSource(fid, GitDirFilter(repoDir.getPath))
        case _ => throw ArtifactSourceCreationException(s"Failed to clone '$owner/$repo'. Return code $rc")
      }
    } catch {
      case e: Exception =>
        throw ArtifactSourceCreationException(s"Failed to clone '$owner/$repo'", e)
    }

  def cleanUp(dir: File): Unit = FileUtils.deleteQuietly(dir)

  def cleanUp(fid: FileSystemArtifactSourceIdentifier): Unit = cleanUp(fid.rootFile)

  def resetDirectoryContent(dir: File): Unit = FileUtils.cleanDirectory(dir)

  def resetDirectoryContent(fid: FileSystemArtifactSourceIdentifier): Unit = resetDirectoryContent(fid.rootFile)

  private def createRepoDirectory(repo: String, owner: String, dir: Option[File]) =
    dir match {
      case Some(file) =>
        Try(Files.createDirectory(file.toPath)) match {
          case Success(path) => path.toFile
          case Failure(e: FileAlreadyExistsException) =>
            resetDirectoryContent(file)
            file
        }
      case None =>
        val tempDir = Files.createTempDirectory(s"${owner}_${repo}_${System.currentTimeMillis}").toFile
        tempDir.deleteOnExit()
        tempDir
    }

  private def getCloneCommands(repo: String, owner: String, branch: Option[String], depth: Int, path: String): Seq[String] = {
    val branchSeq = branch match {
      case Some(br) => if (br == "master") Seq.empty else Seq("-b", br)
      case _ => Seq.empty
    }
    Seq("git", "clone") ++ branchSeq ++ Seq("--depth", depth + "", s"$getUrl/$owner/$repo.git", path)
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
