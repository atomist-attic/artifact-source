package com.atomist.source.git.github

import com.atomist.source.{ArtifactSource, ArtifactSourceAccessException}
import com.typesafe.scalalogging.LazyLogging

/**
  * [[GitHubSourceReader]] that tries multiple strategies in a fixed order.
  * Loading will be lazy, so if any reader succeeds its returned [[ArtifactSource]]
  * is immediately returned.
  *
  * @param readers a sequence of GitHubSourceReaders
  */
class MultiStrategyGitHubSourceReader(readers: Seq[GitHubSourceReader])
  extends GitHubSourceReader
    with LazyLogging {

  override def sourceFor(id: GitHubArtifactSourceLocator): ArtifactSource = {
    var found: Option[ArtifactSource] = None
    for {
      r <- readers if found.isEmpty
    }
      try {
        found = Some(r.sourceFor(id))
        logger.debug(s"Reader $r for $id returned ${found.get}")
      }
      catch {
        case t: Throwable =>
          logger.warn(s"Reader $r failed with ${t.getMessage}")
      }
    found.getOrElse(
      throw ArtifactSourceAccessException(s"All ${readers.size} readers failed"))
  }

  override def treeFor(id: GitHubShaIdentifier): ArtifactSource = ???
}
