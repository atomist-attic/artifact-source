package com.atomist.source.filter

case class AtomistIgnoreFileFilter(override val rootPath: String) extends AbstractPathFilter(rootPath) {

  override protected def filePath: String = ".atomist/ignore"
}
