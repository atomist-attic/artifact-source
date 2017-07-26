package com.atomist.source

case class ArtifactSourceException(message: String, cause: Throwable = null)
  extends Exception(message, cause)