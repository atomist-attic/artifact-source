package com.atomist.source

case class ArtifactSourceException(message: String, cause: Throwable)
  extends Exception(message, cause) {

  def this(message: String) = this(message, null)
}

object ArtifactSourceException {

  def apply(message: String) = new ArtifactSourceException(message)
}