package com.atomist.source.file

import java.io.{File, FileInputStream}
import java.nio.file._
import java.nio.file.attribute.PosixFileAttributes
import java.util.regex.Matcher

import com.atomist.source._
import com.atomist.source.filter.ArtifactFilter
import com.atomist.util.FilePermissions

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * Identifies artifacts on the file system.
  */
trait FileSystemArtifactSourceIdentifier extends ArtifactSourceIdentifier {

  override val name: String = s"${rootFile.getPath}"

  /**
    * Returns the location of the content. Could be a directory or a file.
    */
  def rootFile: File
}

object FileSystemArtifactSourceIdentifier {

  def apply(rootFile: File) = SimpleFileSystemArtifactSourceIdentifier(rootFile)
}

case class SimpleFileSystemArtifactSourceIdentifier(rootFile: File)
  extends FileSystemArtifactSourceIdentifier

object FileSystemArtifactSource {

  def apply(id: FileSystemArtifactSourceIdentifier, artifactFilters: Seq[ArtifactFilter]) =
    new FileSystemArtifactSource(id, artifactFilters)

  def apply(id: FileSystemArtifactSourceIdentifier, artifactFilter: ArtifactFilter) =
    new FileSystemArtifactSource(id, Seq(artifactFilter))

  def apply(id: FileSystemArtifactSourceIdentifier) =
    new FileSystemArtifactSource(id)
}

/**
  * ArtifactSource backed by file system.
  */
class FileSystemArtifactSource(val id: FileSystemArtifactSourceIdentifier,
                               val artifactFilters: Seq[ArtifactFilter] = Seq())
  extends ArtifactSource
    with DirectoryBasedArtifactContainer {

  if (!id.rootFile.exists)
    throw ArtifactSourceAccessException(s"File '${id.rootFile}' does not exist")

  private def wrap(f: File): Artifact =
    if (f.isDirectory)
      new LazyFileSystemDirectoryArtifact(f, id.rootFile)
    else
      new LazyFileSystemFileArtifact(f, id.rootFile)

  override lazy val artifacts: Seq[Artifact] = {
    (id.rootFile match {
      case d: File if d.isDirectory => filterFiles(d.listFiles.toSeq).map(wrap)
      case f: File => filterFiles(Seq(f)).map(wrap)
    }).sortBy(a => (a.name, a.pathElements.mkString("/")))
  }

  private def filterFiles(unfilteredFiles: Seq[File]) = {
    @tailrec
    def applyFilter(files: Seq[File], filters: Seq[ArtifactFilter]): Seq[File] = filters match {
      case Nil => files
      case head :: tail => applyFilter(files.filter(f => head(f.getPath)), tail)
    }
    applyFilter(unfilteredFiles, artifactFilters)
  }

  // Can't extend Artifact as it's a sealed trait, so these
  // methods will become subclass implementations of Artifact methods
  private abstract class LazyFileSystemArtifact(f: File, root: File) {

    val name = f.getName

    // Remove path above root
    protected val pathElementsFromFile: Seq[String] = {
      val fixed = f.getPath.replace(root.getPath, "")
      val sep = Matcher.quoteReplacement(File.separator)
      val elts = fixed.split(sep).toSeq
      if (elts.nonEmpty && elts.head.equals("")) elts.drop(1) else elts
    }
  }

  override def toString = s"${super.toString} wrapping $id"

  private class LazyFileSystemDirectoryArtifact(dir: File, root: File)
    extends LazyFileSystemArtifact(dir, root)
      with DirectoryArtifact
      with DirectoryBasedArtifactContainer {

    override val pathElements: Seq[String] = pathElementsFromFile

    override val artifacts: Seq[Artifact] = filterFiles(dir.listFiles.toSeq).map(wrap)

    override def toString = s"Name: '$name':path: '$path' wrapping $dir - ${getClass.getSimpleName}"
  }

  private class LazyFileSystemFileArtifact(val f: File, root: File)
    extends LazyFileSystemArtifact(f, root)
      with StreamedFileArtifact {

    override def contentLength = f.length().toInt

    // The file name isn't part of the path
    override val pathElements: Seq[String] = pathElementsFromFile.dropRight(1)

    override def inputStream() = new FileInputStream(f)

    override def mode =
      Try {
        FilePermissions.toMode(Files.readAttributes(f.toPath, classOf[PosixFileAttributes]).permissions())
      } match {
        case Success(mode) => mode
        case Failure(e: UnsupportedOperationException) =>
          // Windows
          if (Files.isExecutable(f.toPath))
            FileArtifact.ExecutableMode
          else
            FileArtifact.DefaultMode
        case Failure(e) => throw new IllegalArgumentException(e)
      }

    override def toString = s"Name: '$name':path: '$path' wrapping $f - ${getClass.getSimpleName}"
  }

}

/**
  * Loads file artifacts from classpath.
  */
object ClassPathArtifactSource {

  def toArtifactSource(resource: String,
                       artifactFilters: Seq[ArtifactFilter] = Seq()): ArtifactSource = {
    val f = classPathResourceToFile(resource)
    new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f), artifactFilters)
  }

  def classPathResourceToFile(resource: String): File = {
    val r = getClass.getClassLoader.getResource(resource)
    if (r == null)
      throw ArtifactSourceAccessException(s"No classpath resource at '$resource'")

    new File(r.toURI)
  }
}