package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Release(id: Int,
                   @JsonProperty("tag_name") tagName: String,
                   @JsonProperty("target_commitish") targetCommitish: String,
                   name: String,
                   body: String,
                   draft: Boolean,
                   prerelease: Boolean,
                   @JsonProperty("created_at") var createdAt: OffsetDateTime,
                   @JsonProperty("published_at") var publishedAt: OffsetDateTime,
                   @JsonProperty("upload_url") uploadlUrl: String,
                   @JsonProperty("zipball_url") zipballUrl: String,
                   @JsonProperty("tarball_url") tarballUrl: String)