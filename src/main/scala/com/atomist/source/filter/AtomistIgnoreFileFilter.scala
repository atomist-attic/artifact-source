package com.atomist.source.filter

case class AtomistIgnoreFileFilter(override val rootPath: String) extends PatternMatchableArtifactFilter(rootPath) {

  override protected def filePath: String = ".atomist/ignore"
}
