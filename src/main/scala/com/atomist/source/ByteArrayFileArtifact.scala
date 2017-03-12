package com.atomist.source

import java.io.ByteArrayInputStream

import com.atomist.source.FileArtifact.DefaultMode
import com.atomist.util.Utils.{StringImprovements, withCloseable}
import org.apache.commons.io.IOUtils

/**
  * Simple artifact class containing byte array content.
  */
case class ByteArrayFileArtifact(name: String,
                                 pathElements: Seq[String],
                                 bytes: Array[Byte],
                                 override val mode: Int,
                                 override val uniqueId: Option[String])
  extends FileArtifact {

  require(!name.isEmpty, "Name must not be empty")

  def this(name: String, path: String, content: Array[Byte], mode: Int) =
    this(name = name,
      pathElements = if (path == null || "".equals(path)) Nil else path.split("/").toSeq,
      bytes = content,
      mode,
      None)

  def this(name: String, path: String, content: Array[Byte]) =
    this(name, path, content, DefaultMode)

  def this(name: String, pathElements: Seq[String], content: Array[Byte]) =
    this(name, pathElements, content, DefaultMode, None)

  def this(name: String, pathElements: Seq[String], content: Array[Byte], mode: Int) =
    this(name, pathElements, content, mode, None)

  def this(fa: FileArtifact) =
    this(name = fa.name, pathElements = fa.pathElements,
      bytes = withCloseable(fa.inputStream())(is => IOUtils.toByteArray(is)), fa.mode, fa.uniqueId)

  override def isCached = true

  override def content: String = new String(bytes).toSystem

  def contentLength: Long = new String(bytes).toSystem.length.toLong

  override def inputStream() = new ByteArrayInputStream(bytes)

  override def toString = s"${getClass.getSimpleName}:path='[$path]';contentLength=${content.length},uniqueId=$uniqueId"
}

object ByteArrayFileArtifact {

  /**
    * Convenient way to construct ByteArrayFileArtifacts.
    * Forward slashes will be ignored if present.
    * For example, pathName=com/mypackage/filename content=filecontent
    */
  def apply(pathName: String, bytes: Array[Byte], mode: Int): ByteArrayFileArtifact = {
    val npath = NameAndPathElements(pathName)
    ByteArrayFileArtifact(npath.name, npath.pathElements, bytes, mode, None)
  }

  def apply(fa: FileArtifact) = new ByteArrayFileArtifact(fa)

  def toByteArrayFileArtifact(fa: FileArtifact): ByteArrayFileArtifact = fa match {
    case bafa: ByteArrayFileArtifact => bafa
    case fa: FileArtifact => new ByteArrayFileArtifact(fa)
  }

  /**
    * Return an updated version of this file.
    */
  def updated(fa: FileArtifact, newContent: Array[Byte]): ByteArrayFileArtifact =
    toByteArrayFileArtifact(fa).copy(bytes = newContent)

  /**
    * Copy the FileArtifact with a new path.
    */
  def repathed(fa: FileArtifact, pathElements: Seq[String]): ByteArrayFileArtifact =
    toByteArrayFileArtifact(fa).copy(pathElements = pathElements)

  def withNewUniqueId(fa: FileArtifact, id: String): ByteArrayFileArtifact =
    toByteArrayFileArtifact(fa).copy(uniqueId = Some(id))
}
