package com.atomist.source

object EmptyArtifactSource {

  def apply(name: String = "") = new EmptyArtifactSource(name)
}

case class EmptyArtifactSource(id: ArtifactSourceIdentifier)
  extends ArtifactSource
    with DirectoryBasedArtifactContainer {

  override def artifacts: Seq[Artifact] = Nil

  def this(name: String) = this(NamedArtifactSourceIdentifier(name))
}

