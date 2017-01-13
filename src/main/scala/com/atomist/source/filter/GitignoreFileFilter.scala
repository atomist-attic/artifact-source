package com.atomist.source.filter

case class GitignoreFileFilter(override val rootPath: String) extends AbstractPatternFileFilter(rootPath) {

  override protected def filePath: String = ".gitignore"
}
