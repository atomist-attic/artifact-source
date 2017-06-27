package com.atomist.source.git

import java.io.InputStream
import java.nio.charset.Charset

import com.atomist.source._
import com.atomist.source.filter.ArtifactFilter
import com.atomist.util.Octal
import org.apache.commons.io.IOUtils
import org.kohsuke.github.{GHRepository, GHTree, GHTreeEntry}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/**
  * ArtifactSource implementation backed by GitHub Tree API. Lazy loads artifacts.
  */
case class TreeGitHubArtifactSource(id: GitHubShaIdentifier, ghs: GitHubServices, artifactFilters: ArtifactFilter*)
  extends ArtifactSource
    with DirectoryInferringArtifactContainer {

  protected lazy val repository: GHRepository =
    Try(ghs.getRepository(id.repo, id.owner)) match {
      case Success(Some(repo)) => repo
      case Success(None) => throw ArtifactSourceAccessException(s"Failed to find repository '${id.repo}' for owner '${id.owner}'")
      case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
    }

  private class LazyGitHubFileArtifact(te: GHTreeEntry) extends FileArtifact {

    override val name: String = te.getPath.split("/").toSeq.last

    override def uniqueId = Some(te.getSha)

    override def contentLength: Long = te.getSize

    override val pathElements: Seq[String] = te.getPath.split("/").toSeq.dropRight(1)

    override val mode: Int = Octal.octalToInt(te.getMode)

    override lazy val content: String = IOUtils.toString(inputStream, Charset.defaultCharset())

    override def inputStream: InputStream = repository.readBlob(te.getSha)
  }

  override val allFiles: Seq[FileArtifact] = {
    val tree: GHTree = {
      val root = Try(repository.getTreeRecursive(id.sha, 1)) match {
        case Success(t) => t
        case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
      }

      if (id.path == null || id.path.isEmpty) root
      else {
        val treeElement = root.getTree.asScala.find(te => te.getSize == 0 && te.getPath.equals(id.path)).getOrElse(
          throw ArtifactSourceAccessException(s"Path not found: ${id.path}")
        )
        repository.getTreeRecursive(treeElement.getSha, 1)
      }
    }
    if (tree == null || tree.getTree == null)
      throw ArtifactSourceAccessException(s"Failed to retrieve tree for $id")

    if (tree.isTruncated)
      throw ArtifactSourceAccessException(s"Tree is truncated for $id")

    val files = tree.getTree.asScala.filter(_.getType != "tree").map(new LazyGitHubFileArtifact(_))
    if (artifactFilters.isEmpty) files else filterFiles(files)
  }

  private def filterFiles(unfilteredFiles: Seq[FileArtifact]) = {
    @tailrec
    def applyFilter(files: Seq[FileArtifact], filters: List[ArtifactFilter]): Seq[FileArtifact] = filters match {
      case Nil => files
      case head :: tail => applyFilter(files.filter(f => head(f.path)), tail)
    }

    applyFilter(unfilteredFiles, artifactFilters.toList)
  }
}

object TreeGitHubArtifactSource {

  def apply(asl: GitHubArtifactSourceLocator, ghs: GitHubServices, artifactFilters: ArtifactFilter*): TreeGitHubArtifactSource =
    Try(ghs.getRepository(asl.repo, asl.owner)) match {
      case Success(Some(repository)) =>
        Try(repository.getBranch(asl.branch)) match {
          case Success(branch) => new TreeGitHubArtifactSource(GitHubArtifactSourceIdentifier(asl, branch.getSHA1), ghs, artifactFilters: _*)
          case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
        }
      case Success(None) => throw ArtifactSourceAccessException(s"Failed to find repository '${asl.repo}' for owner '${asl.owner}'")
      case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
    }
}