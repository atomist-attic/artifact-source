package com.atomist.source.git.github.domain

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.annotation.JsonProperty.Access

case class TreeEntry(path: String,
                     mode: String,
                     `type`: String,
                     @JsonProperty(access = Access.WRITE_ONLY) size: Int,
                     sha: String,
                     @JsonProperty(access = Access.WRITE_ONLY) url: String) {
}

object TreeEntry {

  def apply(path: String, mode: String, `type`: String, sha: String): TreeEntry =
    TreeEntry(path, mode, `type`, -1, sha, "")
}
