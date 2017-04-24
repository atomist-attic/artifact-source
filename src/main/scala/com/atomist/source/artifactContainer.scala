package com.atomist.source

import java.util.{List => JList}

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

  def artifactsAsJava: JList[Artifact] = artifacts.asJava

  def empty: Boolean = artifacts.isEmpty

  def directories: Seq[DirectoryArtifact] = artifacts collect {
    case da: DirectoryArtifact => da
  }

  def directoriesAsJava: JList[DirectoryArtifact] = directories.asJava

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

  def findFile(pathName: String): Option[FileArtifact] = {
    val rel = if (pathName.startsWith("/")) pathName.drop(1) else pathName
    val split = rel.split("/")
    val name = split.last
    val pathElements = split.seq.dropRight(1)
    findFileByPath(name, pathElements)
  }

  def findDirectoryByPath(pathElements: Seq[String]): Option[DirectoryArtifact] = {
    val relPath = relativeToFullPath(pathElements)
    val allD = allDirectories
    allD.find(d => {
         println(d.pathElements.size +": " + d.pathElements.mkString("/"))
        println(relPath.size + ": " + relPath.mkString("/"))
      d.pathElements.containsSlice(relPath)
    })
  }

  def findFileByPath(name: String, pathElements: Seq[String]): Option[FileArtifact] = {
    val relPath = relativeToFullPath(pathElements)
    val all = allFiles
    all.find(f => {
   //   println(f.pathElements.size +": " + f.pathElements.mkString("/"))
    //  println(relPath.size + ": " + relPath.mkString("/"))
      f.name.equals(name) && f.pathElements == relPath
    })
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
  def files: Seq[FileArtifact] = artifacts.collect {
    case fa: FileArtifact => fa
  }

  def filesAsJava: JList[FileArtifact] = files.asJava

  /**
    * All files.
    *
    * @return all file artifacts, ignoring directory structure,
    *         which will still be available from each FileArtifact.
    */
  def allFiles: Seq[FileArtifact]

  def allFilesAsJava: JList[FileArtifact] = allFiles.asJava

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

  def allDirectoriesAsJava: JList[DirectoryArtifact] = allDirectories.asJava
}

/**
  * Subtrait of ArtifactContainer in which all paths are from the root.
  */
trait RootArtifactContainer extends ArtifactContainer {

  override final protected def relativeToFullPath(pathElements: Seq[String]): Seq[String] = pathElements
}
