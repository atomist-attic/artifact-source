package com.atomist.source.filter

case class DotGitignoreFileFilter(override val rootPath: String) extends AbstractPatternFileFilter(rootPath) {

  override protected def filePath: String = ".gitignore"
}
