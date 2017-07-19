package com.atomist.source.git.github.domain

case class CommitResponse(sha: String, url: String, tree: GitHubRef, message: String, parents: Seq[GitHubRef])