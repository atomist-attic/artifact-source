package com.atomist.source.filter

case class DotGitignoreFileFilter(override val rootPath: String) extends AbstractPathFilter(rootPath) {

  override protected def filePath: String = ".gitignore"
}
