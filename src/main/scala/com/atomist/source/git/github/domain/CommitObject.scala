package com.atomist.source.git.github.domain

case class CommitObject(url: String, author: Committer, committer: Committer, message: String, tree: GitHubRef)
