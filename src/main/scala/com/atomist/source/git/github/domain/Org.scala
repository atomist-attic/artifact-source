package com.atomist.source.git.github.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Org(login: String,
               id: Int,
               url: String,
               description: String,
               name: String,
               company: String,
               @JsonProperty("public_repos") publicRepos: Int,
               followers: Int,
               @JsonProperty("html_url") htmlUrl: String,
               @JsonProperty("created_at") createdAt: OffsetDateTime,
               @JsonProperty("type") `type`: String,
               @JsonProperty("site_admin") siteAdmin: String)
