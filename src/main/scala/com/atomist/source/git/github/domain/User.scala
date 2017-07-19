package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class User(login: String,
                id: Int,
                url: String,
                @JsonProperty("html_url") htmlUrl: String,
                @JsonProperty("type") `type`: String,
                @JsonProperty("site_admin") siteAdmin: String)
