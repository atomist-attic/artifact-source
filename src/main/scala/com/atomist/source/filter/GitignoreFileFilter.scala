package com.atomist.source.filter

case class GitignoreFileFilter(override val rootPath: String) extends PatternMatchableArtifactFilter(rootPath) {

  override protected def filePath: String = ".gitignore"
}
