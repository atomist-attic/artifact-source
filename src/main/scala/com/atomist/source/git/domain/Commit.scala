package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Commit(url: String,
                  sha: String,
                  @JsonProperty("html_url") htmlUrl: String,
                  commit: CommitObject,
                  author: User,
                  committer: User,
                  parents: Seq[GitHubRef])

case class CommitObject(url: String, author: Committer, committer: Committer, message: String, tree: GitHubRef)

case class Committer(name: String, email: String, date: OffsetDateTime)
