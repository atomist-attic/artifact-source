package com.atomist.source.git.domain

case class CommitObject(url: String, author: Committer, committer: Committer, message: String, tree: GitHubRef)
