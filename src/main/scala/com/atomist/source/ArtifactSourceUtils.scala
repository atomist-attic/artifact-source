package com.atomist.source

/**
  * Convenience methods for working with Artifacts and ArtifactSources.
  */
object ArtifactSourceUtils {

  def prettyPrint(as: ArtifactSource): String =
    s"${as.id}\n${prettyPrintArtifacts(as.artifacts)}"

  def prettyPrintDirectory(da: DirectoryArtifact, depth: Int = 1): String =
    s"${da.name}\n${prettyPrintArtifacts(da.artifacts, depth)}"

  def prettyPrintArtifacts(artifacts: Seq[Artifact], depth: Int = 0): String = {
    artifacts.map {
      case da: DirectoryArtifact =>
        s"${Array.fill(depth)("\t").mkString}${da.name}\n${prettyPrintArtifacts(da.artifacts, depth + 1)}"
      case fa: FileArtifact =>
        s"${Array.fill(depth)("\t").mkString}${fa.name}(f)"
    }.mkString("\n")
  }

  def prettyListFiles(ac: ArtifactContainer): String =
    s"${ac.totalFileCount} files, ${ac.allDirectories.size} directories. Files:\n\t" +
      ac.allFiles.mkString("\n\t")
}






