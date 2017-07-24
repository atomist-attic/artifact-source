package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Issue(number: Int,
                 id: Int,
                 title: String,
                 url: String,
                 body: String,
                 user: User,
                 assignee: Option[User],
                 labels: Array[IssueLabel],
                 milestone: Option[Milestone],
                 state: String,
                 @JsonProperty("pull_request") pullRequest: Option[IssuePullRequest],
                 repository: Option[IssueRepository],
                 @JsonProperty("created_at") createdAt: OffsetDateTime,
                 @JsonProperty("updated_at") updatedAt: OffsetDateTime,
                 @JsonProperty("closed_at") closedAt: Option[OffsetDateTime],
                 assignees: Seq[User])

case class IssueLabel(url: String, name: String, color: String)

case class Milestone(url: String, id: Integer, number: Integer)

case class IssuePullRequest(url: String)

case class IssueRepository(@JsonProperty("pushed_at") pushedAt: OffsetDateTime)

case class Comment(id: Int,
                   @JsonProperty("html_url") url: String,
                   body: String,
                   user: User,
                   @JsonProperty("created_at") createdAt: OffsetDateTime,
                   @JsonProperty("updated_at") updatedAt: OffsetDateTime)