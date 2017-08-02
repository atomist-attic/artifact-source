package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class ReviewComment(url: String,
                         id: Int,
                         @JsonProperty("pull_request_review_id") pullRequestReviewId: Int,
                         @JsonProperty("diff_hunk") diffHunk: String,
                         path: String,
                         position: Int,
                         @JsonProperty("original_position") originalPosition: Int,
                         @JsonProperty("commit_id") commitId: String,
                         @JsonProperty("original_commit_id") originalCommitId: String,
                         user: User,
                         body: String,
                         @JsonProperty("created_at") createdAt: OffsetDateTime,
                         @JsonProperty("updated_at") updatedAt: OffsetDateTime,
                         @JsonProperty("html_url") htmlUrl: String,
                         @JsonProperty("pull_request_url") pullRequestUrl: String,
                         @JsonProperty("_links") links: Links)
