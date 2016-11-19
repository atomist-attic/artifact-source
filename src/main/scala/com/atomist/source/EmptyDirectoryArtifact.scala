package com.atomist.source

case class EmptyDirectoryArtifact(name: String, parentPath: Seq[String] = Nil)
  extends DirectoryArtifact
    with DirectoryBasedArtifactContainer {

  override val pathElements: Seq[String] = parentPath :+ name

  override val artifacts: Seq[Artifact] = Nil
}