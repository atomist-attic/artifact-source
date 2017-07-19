package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

private[github] case class CreateReviewComment(body: String,
                                       @JsonProperty("commit_id") commitId: String,
                                       path: String,
                                       position: Int)

