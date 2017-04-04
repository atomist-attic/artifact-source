package com.atomist.source

/**
  * Implements artifacts() method from allFiles and allDirectories, which must be supplied by subclasses.
  */
trait FileBasedArtifactContainer extends ArtifactContainer {

  /**
    * Return all artifacts at this level.
    */
  override lazy val artifacts: Seq[Artifact] =
  // Build from allFiles and directories.
    (allFiles ++ allDirectories).filter(a => relativeToFullPath(Nil).equals(a.parentPathElements))
}

/**
  * Extends FileBasedArtifactContainer to infer directories from list of all files.
  */
trait DirectoryInferringArtifactContainer extends FileBasedArtifactContainer {

  /**
    * All directories, including nested directories.
    */
  override lazy val allDirectories: Seq[DirectoryArtifact] = {
    // Build from allFiles
    // With x/y/z.txt, x/y/z1/other.txt, foo.txt, b/c.txt will return b, x, x/y, x/y/z1, x/y/z1/other
    val allPaths: Set[Seq[String]] = {
      def pathsUnder(path: Seq[String]): Set[Seq[String]] = {
        val pathsBelow: Set[Seq[String]] = allFiles
          .filter(f => f.parentPathElements.nonEmpty && path.forall(f.parentPathElements.contains(_)))
          .map(_.pathElements)
          .filter(_.nonEmpty)
          .toSet

        def pathsAbove(pathElements: Seq[String]): Set[Seq[String]] =
          if (pathElements.size < 1) Set()
          else Set(pathElements) ++ pathsAbove(pathElements.dropRight(1))

        val thePathsAbove = pathsBelow.flatMap(pathsAbove)

        def str(paths: Set[Seq[String]]) =
          paths.map(_.mkString("/")).mkString("[", ",", "]")

        pathsBelow ++ thePathsAbove
      }

      pathsUnder(Nil)
    }

    val directoriesInferred: Seq[DirectoryArtifact] =
      allPaths.map(pathElements =>
        LightweightDirectoryArtifact(
          name = pathElements.last,
          pathElements = pathElements,
          uniqueId = None)
      ).toList

    directoriesInferred
  }

  // Directory artifact returned by inference.
  private case class LightweightDirectoryArtifact(name: String,
                                                  pathElements: Seq[String],
                                                  override val uniqueId: Option[String])
    extends DirectoryArtifact
      with FileBasedArtifactContainer {

    require(!"".equals(name), "Name must be supplied")

    /**
      * All files.
      *
      * @return all file artifacts, ignoring directory structure,
      *         which will still be available from each FileArtifact.
      */
    override def allFiles: Seq[FileArtifact] =
      DirectoryInferringArtifactContainer.this.allFiles.filter(_.parentPathElements.startsWith(pathElements))

    /**
      * All directories, including nested directories.
      */
    override def allDirectories: Seq[DirectoryArtifact] =
      DirectoryInferringArtifactContainer.this.allDirectories.filter(_.parentPathElements.equals(pathElements))
  }
}

/**
  * Convenient artifact container built from files, and inferring directories.
  *
  * @param pAllFiles all files in the ArtifactContainer
  */
class SimpleFileBasedArtifactContainer(pAllFiles: Seq[FileArtifact])
  extends DirectoryInferringArtifactContainer
    with RootArtifactContainer {

  override val allFiles = pAllFiles
}

object FileBasedArtifactContainer {

  def apply(allFiles: Seq[FileArtifact]) = new SimpleFileBasedArtifactContainer(allFiles)
}

/**
  * Convenient way to create an ArtifactSource given files.
  */
class SimpleFileBasedArtifactSource(val id: ArtifactSourceIdentifier,
                                    allFiles: Seq[FileArtifact],
                                    override val cachedDeltas: Seq[Delta] = Nil,
                                    override val collisions: Seq[Delta] = Nil)
  extends SimpleFileBasedArtifactContainer(allFiles)
    with ArtifactSource {

  def this(id: String, allFiles: Seq[FileArtifact]) = this(NamedArtifactSourceIdentifier(id), allFiles)

  def this(id: String, fa: FileArtifact) = this(NamedArtifactSourceIdentifier(id), Seq(fa))

  def this(id: ArtifactSourceIdentifier, fa: FileArtifact) = this(id, Seq(fa))
}

object SimpleFileBasedArtifactSource {

  def apply(files: FileArtifact*) = new SimpleFileBasedArtifactSource("", files)

  def from(as: ArtifactSource) = new SimpleFileBasedArtifactSource(as.id, as.allFiles, as.cachedDeltas, as.collisions)
}