package com.atomist.source

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.Charset

import com.atomist.util.Utils.{StringImprovements, withCloseable}
import org.apache.commons.io.{FilenameUtils, IOUtils}

/**
  * Represents a file or directory artifact.
  */
sealed trait Artifact {

  val name: String

  /**
    * Java file path convention, including name.
    * Does not include initial /.
    */
  def path: String

  /**
    * Identifier, with meaning specific to implementations.
    */
  def uniqueId: Option[String] = None

  /**
    * Directory path above this artifact. May be empty if we are in the root.
    * If it's a directory, will include the name of the directory.
    */
  val pathElements: Seq[String]

  def parentPathElements: Seq[String]

  def isInRoot: Boolean = pathElements.isEmpty
}

trait FileArtifact extends Artifact {

  import FileArtifact._

  /**
    * Is the content cached in this artifact?
    */
  def isCached: Boolean = false

  override def path: String = (if (pathElements.nonEmpty) pathElements.mkString("/") + "/" else "") + name

  /**
    * May not be efficient if streamed.
    */
  def content: String

  def contentLength: Long

  /**
    * Returns an InputStream for this resource. Should be closed after use.
    */
  def inputStream(): InputStream

  def mode: Int = DefaultMode

  override final def equals(o: Any): Boolean = o match {
    case fa: FileArtifact =>
      fa.path.equals(this.path) && sameContentsAs(fa)
    case _ => false
  }

  override def hashCode(): Int = path.hashCode + content.hashCode

  override final def parentPathElements: Seq[String] = pathElements

  protected def sameContentsAs(fa: FileArtifact): Boolean = {
    val sf1 = StringFileArtifact(this)
    val sf2 = StringFileArtifact(fa)
    sf1.content.equals(sf2.content)
  }

  def withContent(newContent: String) =
    StringFileArtifact(this.name, this.pathElements, newContent, this.mode, this.uniqueId)

  def withContent(newContent: Array[Byte]) =
    ByteArrayFileArtifact(this.name, this.pathElements, newContent, this.mode, this.uniqueId)

  def withPath(pathName: String): FileArtifact = this match {
    case _: StringFileArtifact =>
      StringFileArtifact(pathName, this.content, this.mode, this.uniqueId)
    case _ =>
      val base = ByteArrayFileArtifact(this)
      val npath = NameAndPathElements(pathName)
      base.copy(name = npath.name, pathElements = npath.pathElements)
  }

  def withMode(mode: Int): FileArtifact = this match {
    case _: StringFileArtifact =>
      StringFileArtifact(this.name, this.pathElements, this.content, mode, this.uniqueId)
    case _ =>
      val base = ByteArrayFileArtifact(this)
      base.copy(mode = mode)
  }

  def withUniqueId(id: String): FileArtifact = this match {
    case _: StringFileArtifact =>
      StringFileArtifact(this.name, this.pathElements, this.content, mode, Some(id))
    case _ =>
      val base = ByteArrayFileArtifact(this)
      base.copy(uniqueId = Some(id))
  }

  override def toString = s"${getClass.getSimpleName}:path='[$path]';contentLength=${content.length},mode=$mode,uniqueId=$uniqueId"
}

object FileArtifact {

  val DefaultMode = 33188
  val ExecutableMode = 33261

  def validatePath(path: String): Unit =
    require(path != null && !path.startsWith("./") && FilenameUtils.normalize(path) != null, "Path must not be a relative path")
}

trait StreamedFileArtifact extends FileArtifact {

  final override def content: String =
    withCloseable(inputStream())(IOUtils.toString(_, Charset.defaultCharset()).toSystem)
}

trait NonStreamedFileArtifact extends FileArtifact {

  final override def inputStream() =
    new ByteArrayInputStream(content.toSystem.getBytes())
}

trait DirectoryArtifact extends Artifact with ArtifactContainer {

  override def path: String = if (pathElements.nonEmpty) pathElements.mkString("/") else ""

  override final def parentPathElements: Seq[String] = pathElements dropRight 1

  override protected def relativeToFullPath(pathElements: Seq[String]): Seq[String] =
    this.pathElements ++ pathElements

  override def toString: String =
    s"${getClass.getSimpleName}(${System.identityHashCode(this)}):path(${pathElements.size})='${pathElements.mkString(",")}';" +
      s"${artifacts.size} artifacts=[${artifacts.map(_.name).mkString(",")}]"
}

/**
  * Usable when constructing new FileArtifact instances.
  */
case class NameAndPathElements(name: String, pathElements: Seq[String])

object NameAndPathElements {

  def apply(pathName: String): NameAndPathElements = {
    require(Option(pathName).exists(_.trim.nonEmpty), "Path may not be null or empty")

    val stripped = if (pathName.startsWith("/")) pathName.substring(1) else pathName
    require(!stripped.isEmpty, "Path may not contain only /")

    val splitPath = stripped.split("/")
    val name = splitPath.last
    val pathElements = splitPath.dropRight(1).toSeq
    NameAndPathElements(name, pathElements)
  }
}