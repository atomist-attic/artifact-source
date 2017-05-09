package com.atomist.source.git

import java.io.File
import java.net.URL
import java.nio.file.{FileAlreadyExistsException, Files, Path}

import com.atomist.source.ArtifactSourceCreationException
import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier, NamedFileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case class GitRepositoryCloner(oAuthToken: String, remoteUrl: Option[String] = None) {

  import GitRepositoryCloner._

  @throws[ArtifactSourceCreationException]
  def clone(repo: String,
            owner: String,
            branch: Option[String] = None,
            sha: Option[String] = None,
            dir: Option[File] = None,
            depth: Int = Depth): FileSystemArtifactSource = {
    val repoDir = Try(createRepoDirectory(repo, owner, dir)) match {
      case Success(file) => file
      case Failure(e) =>
        throw new ArtifactSourceCreationException(s"Failed to create target directory for '$owner/$repo'", e)
    }

    val cloneCommands = getCloneCommands(repo, owner, branch, depth, repoDir.getPath)
    val rc = runCommands(repoDir, cloneCommands: _*)
    rc match {
      case 0 =>
        sha match {
          case Some(commitSha) =>
            val resetCommands = Seq("git", "reset", "--hard", commitSha)
            val rc2 = runCommands(repoDir, resetCommands: _*)
            rc2 match {
              case 0 =>
              case _ =>
                val rc3 = runCommands(repoDir, "git", "config", "remote.origin.fetch", "+refs/heads/*:refs/remotes/origin/*")
                rc3 match {
                  case 0 =>
                    val rc4 = runCommands(repoDir, "git", "fetch", "--unshallow")
                    rc4 match {
                      case 0 => runCommands(repoDir, resetCommands: _*)
                      case _ =>
                        throw ArtifactSourceCreationException(s"Failed to fetch '$owner/$repo'. Return code $rc4")
                    }
                  case _ =>
                    throw ArtifactSourceCreationException(s"Failed to find commit with sha $commitSha. Return code $rc3")
                }
            }
          case None =>
        }
        val fid = NamedFileSystemArtifactSourceIdentifier(repo, repoDir)
        FileSystemArtifactSource(fid, GitDirFilter(repoDir.getPath))
      case _ => throw ArtifactSourceCreationException(s"Failed to clone '$owner/$repo'. Return code $rc")
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

  private def runCommands(repoDir: File, commands: String*): Int = {
    // println(commands.mkString(" "))
    new ProcessBuilder(commands.asJava).directory(repoDir).start.waitFor
  }

  private def getCloneCommands(repo: String,
                               owner: String,
                               branch: Option[String],
                               depth: Int,
                               path: String): Seq[String] = {
    val commands = ListBuffer[String]("git", "clone")
    branch match {
      case Some(br) => if (br != "master") commands ++= Seq("-b", br)
      case _ =>
    }
    commands ++= Seq("--depth", depth + "", "--single-branch", s"$getUrl/$owner/$repo.git", path)
    commands
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
