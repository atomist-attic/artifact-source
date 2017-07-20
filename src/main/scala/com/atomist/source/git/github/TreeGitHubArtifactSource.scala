package com.atomist.source.git.github

import java.io.InputStream
import java.nio.charset.Charset

import com.atomist.source._
import com.atomist.source.filter.ArtifactFilter
import com.atomist.source.git.github.domain.{Repo, Tree, TreeElement}
import com.atomist.util.Octal
import org.apache.commons.io.IOUtils
import resource._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * ArtifactSource implementation backed by GitHub Tree API. Lazy loads artifacts.
  */
case class TreeGitHubArtifactSource(id: GitHubShaIdentifier, ghs: GitHubServices, artifactFilters: ArtifactFilter*)
  extends ArtifactSource
    with DirectoryInferringArtifactContainer {

  protected lazy val repository: Repo =
    Try(ghs getRepository(id.repo, id.owner)) match {
      case Success(Some(repo)) => repo
      case Success(None) => throw ArtifactSourceAccessException(s"Failed to find repository '${id.repo}' for owner '${id.owner}'")
      case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
    }

  private class LazyGitHubFileArtifact(te: TreeElement) extends FileArtifact {

    override val name: String = te.path.split("/").toSeq.last

    override def uniqueId = Some(te.sha)

    override def contentLength: Long = te.size

    override val pathElements: Seq[String] = te.path.split("/").toSeq.dropRight(1)

    override val mode: Int = Octal.octalToInt(te.mode)

    override lazy val content: String =
      managed(inputStream()).acquireAndGet(IOUtils.toString(_, Charset.defaultCharset()))

    override def inputStream(): InputStream = ghs.readBlob(id.repo, id.owner, te.sha)
  }

  override val allFiles: Seq[FileArtifact] = {
    val tree: Tree = {
      val root = Try(ghs.getTreeRecursive(id.repo, id.owner, id.sha)) match {
        case Success(t) => t
        case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
      }

      if (id.path == null || id.path.isEmpty) root
      else {
        val treeElement = root.tree.find(te => te.size == 0 && te.path == id.path).getOrElse(
          throw ArtifactSourceAccessException(s"Path not found: ${id.path}")
        )
        ghs.getTreeRecursive(id.repo, id.owner, treeElement.sha)
      }
    }
    if (tree == null || tree.tree == null)
      throw ArtifactSourceAccessException(s"Failed to retrieve tree for $id")

    if (tree.truncated)
      throw ArtifactSourceAccessException(s"Tree is truncated for $id")

    val files = tree.tree.filter(_.`type` == "blob").map(new LazyGitHubFileArtifact(_))
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
    Try(ghs.getBranch(asl.repo, asl.owner, asl.branch)) match {
      case Success(branch) =>
        new TreeGitHubArtifactSource(GitHubArtifactSourceIdentifier(asl, branch.commit.sha), ghs, artifactFilters: _*)
      case Failure(e) => throw ArtifactSourceAccessException(e.getMessage, e)
    }
}