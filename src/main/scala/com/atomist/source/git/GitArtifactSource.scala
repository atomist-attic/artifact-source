package com.atomist.source.git

import java.nio.file.{Files, Paths}

import com.atomist.source.file.{FileSystemArtifactSource, FileSystemArtifactSourceIdentifier}
import com.atomist.source.filter.GitDirFilter

/**
  * ArtifactSource backed by a Git repository on a file system.
  */
case class GitArtifactSource(override val id: FileSystemArtifactSourceIdentifier)
  extends FileSystemArtifactSource(id, GitDirFilter(id.rootFile.getPath)) {

  require(Files.exists(Paths.get(id.rootFile.getPath, ".git")), ".git directory must exist")
}