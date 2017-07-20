package com.atomist.source.git.github.domain

private[github] case class CreateCommitResponse(sha: String,
                                                url: String,
                                                tree: GitHubRef,
                                                message: String,
                                                parents: Seq[GitHubRef])