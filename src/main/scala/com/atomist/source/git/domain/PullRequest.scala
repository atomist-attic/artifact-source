package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class PullRequest(id: Int,
                       url: String,
                       @JsonProperty("html_url") var htmlUrl: String,
                       number: Int,
                       title: String,
                       body: String,
                       @JsonProperty("created_at") createdAt: OffsetDateTime,
                       @JsonProperty("updated_at") updatedAt: OffsetDateTime,
                       @JsonProperty("merged_at") mergedAt: OffsetDateTime,
                       @JsonProperty("closed_at") closedAt: OffsetDateTime,
                       head: PullRequestObject,
                       base: PullRequestObject,
                       state: String,
                       merged: Boolean,
                       mergeable: Boolean,
                       @JsonProperty("mergeable_state") mergeableState: String,
                       comments: Int,
                       @JsonProperty("review_comments") reviewComments: Int) {

  import PullRequest._

  def isOpen: Boolean = Open == state
}

object PullRequest {

  val Open = "open"
  val Closed = "closed"
  val All = "all"
}
