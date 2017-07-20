package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class CreateRepoRequest(@JsonProperty("name") repo: String,
                             owner: String,
                             description: String,
                             @JsonProperty("private") privateFlag: Boolean = false,
                             @JsonProperty("has_issues") issues: Boolean = true,
                             @JsonProperty("auto_init") autoInit: Boolean = false)