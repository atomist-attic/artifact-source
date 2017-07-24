package com.atomist.source.git

import com.atomist.source.RepoArtifactSourceLocator

trait GitArtifactSourceLocator extends RepoArtifactSourceLocator

object GitArtifactSourceLocator {

  val MasterBranch: String = "master"
}