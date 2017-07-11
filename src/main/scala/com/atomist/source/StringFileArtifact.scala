package com.atomist.source

import com.atomist.source.FileArtifact.{DefaultMode, _}
import com.atomist.util.Utils.StringImprovements

/**
  * Simple artifact class containing content.
  */
case class StringFileArtifact(name: String,
                              pathElements: Seq[String],
                              private val _content: String,
                              override val mode: Int,
                              override val uniqueId: Option[String])
  extends NonStreamedFileArtifact {

  require(!name.isEmpty, "Name must not be empty")

  override def isCached = true

  def contentLength: Long = _content.toSystem.length

  override def content: String = _content.toSystem

  override def toString = s"${getClass.getSimpleName}:path='[$path]';contentLength=$contentLength,mode=$mode,uniqueId=$uniqueId"
}

object StringFileArtifact {

  /**
    * Convenient way to construct StringFileArtifacts.
    * Forward slashes will be ignored if present and relative paths are not permitted.
    * For example, pathName=com/mypackage/filename content=filecontent
    */
  def apply(pathName: String, content: String, mode: Int, uniqueId: Option[String]): StringFileArtifact = {
    validatePath(pathName)
    val npath = NameAndPathElements(pathName)
    StringFileArtifact(npath.name, npath.pathElements, content, mode, uniqueId)
  }

  def apply(pathName: String, content: String): StringFileArtifact =
    apply(pathName, content, DefaultMode, None)

  /**
    * For example, name=filename path=com/mypackage content=filecontent
    */
  def apply(name: String, path: String, content: String) = {
    validatePath(path)
    new StringFileArtifact(name = name,
      pathElements = if (Option(path).exists(_.trim.nonEmpty)) path.split("/").toSeq else Nil,
      _content = content,
      mode = DefaultMode,
      None)
  }

  def apply(name: String, pathElements: Seq[String], content: String) =
    new StringFileArtifact(name, pathElements, content, DefaultMode, None)

  def apply(fa: FileArtifact) = fa match {
    case sfa: StringFileArtifact => sfa
    case fa: FileArtifact => new StringFileArtifact(name = fa.name, pathElements = fa.pathElements,
      _content = fa.content, mode = fa.mode, fa.uniqueId)
  }

  /**
    * Return an updated version of this file with new content.
    */
  def updated(fa: FileArtifact, newContent: String) =
    StringFileArtifact(fa.name, fa.pathElements, newContent, fa.mode, fa.uniqueId)

  /**
    * Copy the FileArtifact with a new path.
    */
  def repathed(fa: FileArtifact, pathElements: Seq[String]) =
    StringFileArtifact(fa.name, pathElements, fa.content, fa.mode, fa.uniqueId)

  def withNewUniqueId(fa: FileArtifact, id: String) =
    StringFileArtifact(fa.name, fa.pathElements, fa.content, fa.mode, Some(id))
}
