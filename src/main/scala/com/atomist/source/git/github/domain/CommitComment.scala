package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class CommitComment(@JsonProperty("html_url") htmlUrl: String,
                         url: String,
                         id: Int,
                         body: String,
                         path: String,
                         position: Int,
                         line: Int,
                         @JsonProperty("commit_id") commitId: String,
                         user: User,
                         @JsonProperty("created_at") createdAt: OffsetDateTime,
                         @JsonProperty("updated_at") updatedAt: OffsetDateTime)
