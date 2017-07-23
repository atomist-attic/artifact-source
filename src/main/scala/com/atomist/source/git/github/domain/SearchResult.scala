package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty

private[github] case class SearchResult[T: Manifest](@JsonProperty("total_count") totalCount: Int,
                                                     @JsonProperty("incomplete_results") incompleteResults: Boolean,
                                                     items: Seq[T])
