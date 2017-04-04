package com.atomist.source

import scala.collection.JavaConverters._

/**
  * Extended by classes that hold a number of artifacts,
  * whether an ArtifactSource or a DirectoryArtifact.
  * Implements numerous methods whose implementation does not force a
  * particular data structure; see DirectoryBasedArtifactContainer for
  * a convenient trait to extend for a directory based approach.
  *
  * @see [[DirectoryBasedArtifactContainer]]
  */
trait ArtifactContainer {

  /**
    * Return all artifacts at this level.
    *
    * @return a list of Artifacts
    */
  def artifacts: Seq[Artifact]

  def empty: Boolean = artifacts.isEmpty

  def directories: Seq[DirectoryArtifact] = artifacts collect {
    case da: DirectoryArtifact => da
  }

  /**
    * Find directory under current directory. If the name contains / it will
    * be treated as a path and findDirectoryByPath will be used.
    * A leading / is not valid, and will cause a lookup failure.
    *
    * @param name the name of the directory
    * @return a DirectoryArtifact
    */
  def findDirectory(name: String): Option[DirectoryArtifact] = findDirectoryByPath(name.split("/").seq)

  /**
    * Find a direct child of this directory.
    *
    * @param name the name of the directory
    * @return a DirectoryArtifact
    */
  def findChildDirectory(name: String): Option[DirectoryArtifact] = artifacts.collect {
    case da: DirectoryArtifact if da.name equals name => da
  }.headOption

  // TODO what if it's a directory?
  def findFile(pathName: String): Option[FileArtifact] = {
    val split = pathName.split("/")
    val name = split.last
    val pathElements = split.seq.dropRight(1)
    findFileByPath(name, pathElements)
  }

  def findDirectoryByPath(pathElements: Seq[String]): Option[DirectoryArtifact] = {
    val relPath = relativeToFullPath(pathElements)
    allDirectories.find(_.pathElements.equals(relPath))
  }

  def findFileByPath(name: String, pathElements: Seq[String]): Option[FileArtifact] = {
    val relPath = relativeToFullPath(pathElements)
    allFiles.find(f => f.name.equals(name) && f.pathElements.equals(relPath))
  }

  /**
    * Subclasses should override this to return the full path for the given relative path.
    *
    * @param pathElements elements in relative path
    * @return relative path
    */
  protected def relativeToFullPath(pathElements: Seq[String]): Seq[String]

  /**
    * Return files in top level directory.
    *
    * @return a list files in top level directory
    */
  def files: Seq[FileArtifact] =
    artifacts.collect {
      case fa: FileArtifact => fa
    }

  /**
    * All files.
    *
    * @return all file artifacts, ignoring directory structure,
    * which will still be available from each FileArtifact.
    */
  def allFiles: Seq[FileArtifact]

  def allFilesAsJava: java.util.List[FileArtifact] = allFiles.asJava

  /**
    * Count of all files in all subdirectories.
    *
    * @return count of total number of files
    */
  def totalFileCount = allFiles.size

  /**
    * All directories, including nested directories.
    */
  def allDirectories: Seq[DirectoryArtifact]
}

/**
  * Subtrait of ArtifactContainer in which all paths are from the root.
  */
trait RootArtifactContainer extends ArtifactContainer {

  override final protected def relativeToFullPath(pathElements: Seq[String]): Seq[String] = pathElements
}