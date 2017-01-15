package com.atomist.source

case class SimpleDirectoryArtifact(name: String,
                                   pathElements: Seq[String],
                                   artifacts: Seq[Artifact],
                                   override val uniqueId: Option[String])
  extends DirectoryArtifact
    with DirectoryBasedArtifactContainer {

  def this(da: SimpleDirectoryArtifact) = this(da.name, da.pathElements, da.artifacts, da.uniqueId)
}