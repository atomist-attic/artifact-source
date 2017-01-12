package com.atomist.source.filter

import java.nio.file._

case class DotGitDirFilter(rootPath: String) extends ArtifactFilter {

  private val dotGitDir = Paths.get(rootPath, ".git")

  override def apply(path: String): Boolean =
    !Paths.get(path).toAbsolutePath.startsWith(dotGitDir.toAbsolutePath)
}
