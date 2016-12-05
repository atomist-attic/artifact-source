package com.atomist.source.file

import java.io.{File, FileInputStream}
import java.nio.file._
import java.util.regex.Matcher

import com.atomist.source._
import com.atomist.util.GitignoreUtils

import scala.language.postfixOps

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

/**
  * ArtifactSource backed by file system.
  */
class FileSystemArtifactSource(val id: FileSystemArtifactSourceIdentifier)
  extends ArtifactSource
    with DirectoryBasedArtifactContainer {

  if (!id.rootFile.exists)
    throw ArtifactSourceAccessException(s"File '${id.rootFile}' does not exist")

  private val rootPath = id.rootFile.getPath

  private val ignoredFiles: List[File] =
    GitignoreUtils.ignoredFiles(rootPath, List(s"${System.getProperty("user.home")}/.config/git/ignore"))

  private def matchIgnoredFile(f: File) = ignoredFiles.contains(f)

  private def wrap(f: File): Artifact =
    if (f.isDirectory)
      new LazyFileSystemDirectoryArtifact(f, id.rootFile)
    else
      new LazyFileSystemFileArtifact(f, id.rootFile)

  override lazy val artifacts: Seq[Artifact] =
    (id.rootFile match {
      case d: File if d.isDirectory => d.listFiles.filterNot(f => matchIgnoredFile(f)).map(d => wrap(d)).toSeq
      case f: File => if (matchIgnoredFile(f)) Nil else Seq(wrap(f))
    }).filter {
      case da: LazyFileSystemDirectoryArtifact => da.allFiles.nonEmpty
      case _ => true
    }.sortBy(a => (a.name, a.pathElements.mkString("/")))

  // Can't extend Artifact as it's a sealed trait, so these
  // methods will become subclass implementations of Artifact methods
  private abstract class LazyFileSystemArtifact(f: File, root: File) {

    val name = f.getName

    // Remove path above root
    protected val pathElementsFromFile: Seq[String] = {
      val elts = f.getPath.replace(root.getPath, "").split(Matcher.quoteReplacement(File.separator)).toSeq
      if (elts.nonEmpty && elts.head.equals("")) elts.drop(1) else elts
    }
  }

  override def toString = s"${super.toString} wrapping $id"

  private class LazyFileSystemDirectoryArtifact(dir: File, root: File)
    extends LazyFileSystemArtifact(dir, root)
      with DirectoryArtifact
      with DirectoryBasedArtifactContainer {

    override val pathElements: Seq[String] = pathElementsFromFile

    override val artifacts: Seq[Artifact] =
      dir.listFiles.filterNot(f => matchIgnoredFile(f)).map(f => wrap(f))

    override def toString = s"Name: '$name':path: '$path' wrapping $dir - ${getClass.getSimpleName}"
  }

  private class LazyFileSystemFileArtifact(val f: File, root: File)
    extends LazyFileSystemArtifact(f, root)
      with StreamedFileArtifact {

    override def contentLength = f.length().toInt

    // The file name isn't part of the path
    override val pathElements: Seq[String] = pathElementsFromFile.dropRight(1)

    override def inputStream() = new FileInputStream(f)

    override def mode = if (Files.isExecutable(f.toPath) || f.canExecute) FileArtifact.ExecutableMode else FileArtifact.DefaultMode

    override def toString = s"Name: '$name':path: '$path' wrapping $f - ${getClass.getSimpleName}"
  }
}

/**
  * Loads file artifacts from classpath.
  */
object ClassPathArtifactSource {

  def toArtifactSource(resource: String): ArtifactSource = {
    val f = classPathResourceToFile(resource)
    val fsasid = FileSystemArtifactSourceIdentifier(f)
    new FileSystemArtifactSource(fsasid)
  }

  def classPathResourceToFile(resource: String): File = {
    val r = getClass.getClassLoader.getResource(resource)
    if (r == null)
      throw ArtifactSourceAccessException(s"No classpath resource at '$resource'")
    new File(r.toURI)
  }
}