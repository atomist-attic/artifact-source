package com.atomist.source.git

case class DefaultGitHubArtifactSourceIdentifier(repo: String,
                                                 owner: String,
                                                 path: String = "",
                                                 branch: String = "master",
                                                 commitSha: String)
  extends GitHubArtifactSourceIdentifier {

  override val name: String = s"$owner/$repo"

  override def toString = s"repository '$repo', owner '$owner'"
}
