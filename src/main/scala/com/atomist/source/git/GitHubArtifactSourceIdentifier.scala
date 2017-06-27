package com.atomist.source.git

/**
  * Identifies a source read from a GitHubArtifactSourceLocator.
  * Includes a commit sha.
  */
trait GitHubArtifactSourceIdentifier extends GitHubArtifactSourceLocator with GitHubShaIdentifier {

  /**
    * Sha of the commit.
    *
    * @return the sha
    */
  def commitSha: String

  override def sha = commitSha

  override def equals(o: Any) = o match {
    case that: GitHubArtifactSourceIdentifier =>
      super.equals(that) &&
        commitSha.equals(that.commitSha)
    case _ => false
  }
}

object GitHubArtifactSourceIdentifier {

  def apply(loc: GitHubArtifactSourceLocator, commitSha: String): GitHubArtifactSourceIdentifier =
    DefaultGitHubArtifactSourceIdentifier(loc.repo, loc.owner, loc.path, loc.branch, commitSha)

  /**
    * Extract into repository and owner.
    *
    * @param ghi the GitHub artifact source identifier
    * @return Option tuple of repository,owner
    */
  def unapply(ghi: GitHubArtifactSourceIdentifier): Option[(String, String)] = Some(ghi.repo, ghi.owner)
}