package com.atomist.source

/**
  * Represents a change to an ArtifactSource.
  */
sealed trait Delta {

  def path: String
}

case class FileAdditionDelta(newFile: FileArtifact) extends Delta {

  override def path = newFile.path
}

/**
  * The given file was updated.
  */
case class FileUpdateDelta(oldFile: FileArtifact, updatedFile: FileArtifact) extends Delta {

  override def path = updatedFile.path
}

case class FileDeletionDelta(oldFile: FileArtifact) extends Delta {

  override def path = oldFile.path
}

/**
  * Represents a new empty directory.
  */
case class DirectoryAdditionDelta(newDir: DirectoryArtifact) extends Delta {

  override def path = newDir.path
}

case class DirectoryDeletionDelta(oldDir: DirectoryArtifact) extends Delta {

  override def path = oldDir.path
}

case class Deltas(deltas: Seq[Delta], _collisions: Seq[Delta]) {

  def empty: Boolean = deltas.isEmpty

  /**
    * Return the collisions that resulted from this operation.
    */
  def collisions: Seq[Delta] = {
    if (_collisions.isEmpty)
      deltas.collect {
        case fud: FileUpdateDelta => fud
      }.toList
    else
      _collisions.collect {
        case fad: FileAdditionDelta => fad
        case fud: FileUpdateDelta => fud
      }.toList
  }
}

object Deltas {

  def apply(deltas: Seq[Delta]) = new Deltas(deltas, Nil)
}