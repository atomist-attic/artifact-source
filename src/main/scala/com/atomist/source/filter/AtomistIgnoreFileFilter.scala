package com.atomist.source.filter

case class AtomistIgnoreFileFilter(override val rootPath: String) extends AbstractPatternFileFilter(rootPath) {

  override protected def filePath: String = ".atomist/ignore"
}
