package com.atomist.source.git.github

import com.atomist.source.{ArtifactSource, ArtifactSourceAccessException}

trait GitHubSourceReader {

  /**
    * Return ArtifactSource for the given repository and branch. The returned contents will
    * indicate the sha, which can be saved, but calling with the same [[com.atomist.source.ArtifactSourceLocator]]
    * will produce the same results.
    *
    * @param id the GitHubArtifactSourceLocator
    * @return an ArtifactSource
    * @throws ArtifactSourceAccessException if the ArtifactSource cannot be returned
    */
  @throws[ArtifactSourceAccessException]
  def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource

  /**
    * Return the tree for the given sha. Always produces the same results.
    * Because it also implements [[GitHubShaIdentifier]], a [[GitHubArtifactSourceIdentifier]]
    * can be passed to this method, always returning the same result.
    *
    * @param id the GitHubArtifactSourceLocator
    * @return an ArtifactSource
    * @throws ArtifactSourceAccessException if the tree for given sha cannot be returned
    */
  @throws[ArtifactSourceAccessException]
  def treeFor(id: GitHubShaIdentifier): ArtifactSource
}
