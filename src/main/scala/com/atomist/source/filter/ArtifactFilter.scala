package com.atomist.source.filter

/**
  * Filters artifacts by some String field such as name or path.
  */
trait ArtifactFilter {

  def apply(s: String): Boolean
}
