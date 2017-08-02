package com.atomist.source.git.domain

private[git] case class CreateCommitResponse(sha: String,
                                                url: String,
                                                tree: GitHubRef,
                                                message: String,
                                                parents: Seq[GitHubRef])