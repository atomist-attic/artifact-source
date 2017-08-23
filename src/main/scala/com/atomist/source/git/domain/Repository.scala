package com.atomist.source.git.domain

import java.time.OffsetDateTime

import com.fasterxml.jackson.annotation.JsonProperty

case class Repository(id: Long,
                      name: String,
                      description: String,
                      url: String,
                      @JsonProperty("forks_count") forksCount: Int,
                      owner: RepoOwner,
                      @JsonProperty("pushed_at") pushedAt: OffsetDateTime) {

  def ownerName: String = owner.login
}