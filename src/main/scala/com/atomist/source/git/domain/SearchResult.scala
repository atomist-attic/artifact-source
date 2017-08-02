package com.atomist.source.git.domain

import com.fasterxml.jackson.annotation.JsonProperty

case class SearchResult[T](@JsonProperty("total_count") totalCount: Int,
                           @JsonProperty("incomplete_results") incompleteResults: Boolean,
                           nextPage: Int,
                           lastPage: Int,
                           items: Seq[T])
