package com.atomist.source.git.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class Content(`type`: String,
                   encoding: String,
                   size: Int,
                   name: String,
                   path: String,
                   content: String,
                   sha: String,
                   url: String,
                   @JsonProperty("git_url") gitUrl: String,
                   @JsonProperty("html_url") htmlUrl: String,
                   @JsonProperty("download_url") downloadUrl: String)
