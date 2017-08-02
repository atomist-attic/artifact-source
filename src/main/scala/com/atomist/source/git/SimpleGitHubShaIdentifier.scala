package com.atomist.source.git

case class SimpleGitHubShaIdentifier(repo: String, owner: String, sha: String, path: String)
  extends GitHubShaIdentifier
