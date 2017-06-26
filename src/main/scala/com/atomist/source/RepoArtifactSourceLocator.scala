package com.atomist.source

trait RepoArtifactSourceLocator
  extends ArtifactSourceLocator
    with RepoBranchAndPath {

  def repo: String

  def owner: String
}

