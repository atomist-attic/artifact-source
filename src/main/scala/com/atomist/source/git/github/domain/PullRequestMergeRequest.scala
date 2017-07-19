package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

private[github] case class PullRequestMergeRequest(@JsonProperty("commit_title") title: String,
                                                   @JsonProperty("commit_message") message: String,
                                                   @JsonProperty("merge_method") mergeMethod: String)