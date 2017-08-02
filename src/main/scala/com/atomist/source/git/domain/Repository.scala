package com.atomist.source.git.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class Repository(id: Long,
                      name: String,
                      description: String,
                      url: String,
                      @JsonProperty("forks_count") forksCount: Int,
                      owner: RepoOwner) {

  def ownerName: String = owner.login
}