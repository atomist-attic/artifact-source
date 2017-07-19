package com.atomist.source.git.github

import com.atomist.source.{ArtifactSourceIdentifier, CloudRepoId}

/**
  * Allows us to pick a tree from a given repository.
  * This can guarantee getting the same result every time, unlike
  * using a GitHubSourceIdentifier, which should be used to get the latest in
  * a particular branch.
  *
  * @see [[GitHubArtifactSourceIdentifier]]
  */
trait GitHubShaIdentifier extends ArtifactSourceIdentifier with CloudRepoId {

  def sha: String

  def path: String

  override def name: String = s"$owner/$repo"
}

object GitHubShaIdentifier {

  def apply(repo: String, owner: String, sha: String, path: String = "") =
    SimpleGitHubShaIdentifier(repo, owner, sha, path)
}
