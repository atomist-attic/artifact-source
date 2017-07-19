package com.atomist.source.git.github

import com.atomist.source.git.GitArtifactSourceLocator.MasterBranch

case class DefaultGitHubArtifactSourceLocator(repo: String,
                                              owner: String,
                                              path: String = "",
                                              branch: String = MasterBranch)
  extends GitHubArtifactSourceLocator {

  override val name: String = s"$owner/$repo"

  def this(id: GitHubArtifactSourceLocator) =
    this(id.repo, id.owner, id.path, id.branch)

  /**
    * It's unlikely that we want to change this from the default, so we require an additional method call.
    *
    * @param path new path under the root
    * @return new object with altered path
    */
  def underPath(path: String): DefaultGitHubArtifactSourceLocator = this.copy(path = path)

  override def toString = s"repository '$repo', owner '$owner'"
}