package com.atomist.source.git

import com.atomist.source._

case class GitHubSourceUpdateInfo(sourceId: GitHubArtifactSourceLocator, message: String) extends SourceUpdateInfo {

  override def toString = s"repository '${sourceId.repo}', owner '${sourceId.owner}'"
}
