package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class PullRequestStatus(id: Int,
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

  import PullRequestStatus._

  def isOpen: Boolean = OpenState == state
}

object PullRequestStatus {

  val OpenState: String = "open"
  val ClosedState: String = "closed"
  val All: String = "all"
}
