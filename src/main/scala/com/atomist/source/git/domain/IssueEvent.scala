package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class IssueEvent(id: Int,
                      url: String,
                      actor: IssueEventActor,
                      event: String,
                      @JsonProperty("commit_id") commitId: Option[String],
                      @JsonProperty("commit_url") commitUrl: Option[String],
                      @JsonProperty("created_at") createdAt: OffsetDateTime)

case class IssueEventActor(login: String,
                           id: Int,
                           url: String,
                           `type`: String)