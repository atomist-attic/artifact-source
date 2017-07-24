package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonProperty.Access

case class Tree(sha: String,
                url: String,
                tree: Seq[TreeEntry],
                @JsonProperty(access = Access.WRITE_ONLY) truncated: Boolean)
