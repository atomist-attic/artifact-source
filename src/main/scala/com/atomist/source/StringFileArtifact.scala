package com.atomist.source

import com.atomist.source.FileArtifact.DefaultMode
import com.atomist.util.Utils.StringImprovements

/**
  * Simple artifact class containing content.
  */
case class StringFileArtifact(
                               name: String,
                               pathElements: Seq[String],
                               private val _content: String,
                               override val mode: Int,
                               override val uniqueId: Option[String])
  extends NonStreamedFileArtifact {

  require(!name.isEmpty, "Name must not be empty")

  def this(name: String, path: String, _content: String, mode: Int) =
    this(name = name,
      pathElements = if (path == null || "".equals(path)) Nil else path.split("/").toSeq,
      _content = _content,
      mode = mode,
      None)

  /**
    * For example, name=filename path=com/mypackage content=filecontent
    */
  def this(name: String, path: String, content: String) =
    this(name, path, content, DefaultMode)

  def this(name: String, pathElements: Seq[String], content: String) =
    this(name, pathElements, content, DefaultMode, None)

  def this(name: String, pathElements: Seq[String], content: String, mode: Int) =
    this(name, pathElements, content, mode, None)

  def this(fa: FileArtifact) =
    this(name = fa.name, pathElements = fa.pathElements, _content = fa.content, mode = fa.mode, fa.uniqueId)

  override def isCached = true

  def contentLength: Int = _content.toSystem.length

  override def content : String = _content.toSystem

  override def toString = s"${getClass.getSimpleName}:path='[$path]';contentLength=$contentLength,mode=$mode,uniqueId=$uniqueId"
}

object StringFileArtifact {

  /**
    * Convenient way to construct StringFileArtifacts.
    * Forward slashes will be ignored if present.
    * For example, pathName=com/mypackage/filename content=filecontent
    */
  def apply(pathName: String, content: String, mode: Int, uniqueId: Option[String]): StringFileArtifact = {
    val npath = NameAndPathElements(pathName)
    StringFileArtifact(npath.name, npath.pathElements, content, mode, uniqueId)
  }

  def apply(name: String, pathElements: Seq[String], content: String) =
    new StringFileArtifact(name, pathElements, content)

  def apply(name: String, path: String, content: String) =
    new StringFileArtifact(name, path, content, DefaultMode)

  def apply(pathName: String, content: String): StringFileArtifact =
    apply(pathName, content, DefaultMode, None)

  def toStringFileArtifact(fa: FileArtifact): StringFileArtifact = fa match {
    case sfa: StringFileArtifact => sfa
    case fa: FileArtifact => new StringFileArtifact(fa)
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
