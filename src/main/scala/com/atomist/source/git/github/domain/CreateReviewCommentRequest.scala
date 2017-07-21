package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

private[github] case class CreateReviewCommentRequest(body: String,
                                                      @JsonProperty("commit_id") commitId: String,
                                                      path: String,
                                                      position: Int)

