package com.atomist.source.git

import com.atomist.source.CloudRepoId
import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch

/**
  * Trait for objects that can locate GitHub repos.
  * Different GitHubArtifactSourceIdentifiers can have different shas.
  */
trait GitHubArtifactSourceLocator extends GitArtifactSourceLocator with CloudRepoId {

  override def equals(o: Any) = o match {
    case that: GitHubArtifactSourceLocator =>
      this.owner.equals(that.owner) && this.repo.equals(that.repo) &&
        this.branch.equals(that.branch) && this.path.equals(that.path)
    case _ => false
  }
}

object GitHubArtifactSourceLocator {

  def apply(cri: CloudRepoId, branch: String = MasterBranch) =
    DefaultGitHubArtifactSourceLocator(cri.repo, cri.owner, "", branch)

  def rootOfMaster(repo: String, owner: String): GitHubArtifactSourceLocator = fromStrings(repo, owner)

  def fromStrings(repo: String, owner: String, branch: String = MasterBranch) =
    DefaultGitHubArtifactSourceLocator(repo, owner, "", branch)
}