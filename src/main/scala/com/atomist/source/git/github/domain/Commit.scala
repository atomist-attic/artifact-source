package com.atomist.source.git.github.domain

case class Commit(url: String, sha: String, commit: CommitObject, parents: Seq[GitHubRef])
