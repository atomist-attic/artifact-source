package com.atomist.source

import scala.collection.JavaConverters._

/**
  * Implements ArtifactContainer methods using directory-based approach,
  * where files are found by navigating down through directories.
  */
trait DirectoryBasedArtifactContainer extends ArtifactContainer {

  /**
    * All files.
    *
    * @return all file artifacts, ignoring directory structure,
    *         which will still be available from each FileArtifact.
    */
  override lazy val allFiles: Seq[FileArtifact] = {
    def flatten(arts: Seq[Artifact]): Seq[FileArtifact] =
      arts.flatMap(a => a match {
        case d: DirectoryArtifact => flatten(d.artifacts)
        case f: FileArtifact => Seq(f)
      })

    flatten(artifacts)
  }

  override lazy val allFilesAsJava: java.util.List[FileArtifact] = allFiles.asJava

  /**
    * All directories, including nested directories.
    */
  override lazy val allDirectories: Seq[DirectoryArtifact] = {
    def flatten(arts: Seq[Artifact]): Seq[DirectoryArtifact] =
      (arts collect {
        case d: DirectoryArtifact => Seq(d) ++ flatten(d.artifacts)
      }).flatten

    flatten(artifacts)
  }
}
