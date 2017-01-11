package com.atomist.source

import com.atomist.source.PathFilter.Path

/**
  * Filters artifacts by path.
  */
trait PathFilter {

  def apply(path: Path): Boolean
}

object PathFilter {

  type Path = String
}
