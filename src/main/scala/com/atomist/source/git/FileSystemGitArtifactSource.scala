package com.atomist.source.git

import com.atomist.source.file.{FileSystemArtifactSource, NamedFileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter

import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

/**
  * ArtifactSource backed by a Git repository on a local file system.
  */
case class FileSystemGitArtifactSource(override val id: NamedFileSystemArtifactSourceIdentifier)
  extends FileSystemArtifactSource(id, GitDirFilter(id.rootFile.getPath)) {

  require(Try(Process("git rev-parse --is-inside-work-tree", id.rootFile) ! ProcessLogger(_ => ()) == 0).getOrElse(false), "Not a git repository ")
}