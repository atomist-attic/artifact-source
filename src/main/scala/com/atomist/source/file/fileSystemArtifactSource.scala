package com.atomist.source.file

import java.io.{File, FileInputStream}
import java.nio.file._
import java.nio.file.attribute.PosixFileAttributes
import java.util.regex.Matcher

import com.atomist.source._
import com.atomist.util.FilePermissions

import scala.collection.mutable.ListBuffer
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

  def apply(id: FileSystemArtifactSourceIdentifier, pathFilters: Seq[PathFilter]) =
    new FileSystemArtifactSource(id, pathFilters)

  def apply(id: FileSystemArtifactSourceIdentifier) =
    new FileSystemArtifactSource(id)
}

/**
  * ArtifactSource backed by file system.
  */
class FileSystemArtifactSource(val id: FileSystemArtifactSourceIdentifier,
                               val pathFilters: Seq[PathFilter] = Seq())
  extends ArtifactSource
    with DirectoryBasedArtifactContainer {

  if (!id.rootFile.exists)
    throw ArtifactSourceAccessException(s"File '${id.rootFile}' does not exist")

  private def wrap(f: File): Artifact =
    if (f.isDirectory)
      new LazyFileSystemDirectoryArtifact(f, id.rootFile)
    else
      new LazyFileSystemFileArtifact(f, id.rootFile)

  //  override lazy val artifacts: Seq[Artifact] = {
  //    (id.rootFile match {
  //      case d: File if d.isDirectory => if (pathFilters.map(pf => {
  //        println(s"d: processing filter ${pf.getClass.getSimpleName} for path ${d.getPath}")
  //        pf(d.getPath)
  //      }).exists(_ == false)) d.listFiles.map(wrap).toSeq else Nil
  //      case f: File => if (pathFilters.map(pf => {
  //        println(s"f: processing filter ${pf.getClass.getSimpleName} for path ${f.getPath}")
  //        pf(f.getPath)
  //      }).exists(_ == false)) Seq(wrap(f)) else Nil
  //    }).sortBy(a => (a.name, a.pathElements.mkString("/")))
  //  }

  override lazy val artifacts: Seq[Artifact] = {
    (id.rootFile match {
      case d: File if d.isDirectory => filterFiles(d.listFiles)
      case f: File => filterFiles(Array(f))
    }).sortBy(a => (a.name, a.pathElements.mkString("/")))
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

    override val artifacts: Seq[Artifact] = filterFiles(dir.listFiles)

    override def toString = s"Name: '$name':path: '$path' wrapping $dir - ${getClass.getSimpleName}"
  }

  private def filterFiles(files: Array[File]) = {
    if (pathFilters.isEmpty) files.map(wrap).toSeq
    else {
      val buf = new ListBuffer[File]
      for (pf <- pathFilters) {
        buf ++= files.filter(f => pf(f.getPath))
      }
      println("***** " + buf.mkString("\n"))
      buf.distinct.map(wrap).toSeq
    }
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
                       pathFilters: Seq[PathFilter] = Seq()): ArtifactSource = {
    val f = classPathResourceToFile(resource)
    new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f), pathFilters)
  }

  def classPathResourceToFile(resource: String): File = {
    val r = getClass.getClassLoader.getResource(resource)
    if (r == null)
      throw ArtifactSourceAccessException(s"No classpath resource at '$resource'")

    new File(r.toURI)
  }
}