package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class Repo(id: Long,
                name: String,
                description: String,
                url: String,
                @JsonProperty("forks_count") forksCount: Int,
                owner: RepoOwner)