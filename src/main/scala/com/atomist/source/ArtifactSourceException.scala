package com.atomist.source

abstract class ArtifactSourceException(message: String, cause: Throwable)
  extends Exception(message, cause)

case class ArtifactSourceAccessException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(message: String) = this(message, null)
}

object ArtifactSourceAccessException {

  def apply(message: String) = new ArtifactSourceAccessException(message)
}

case class ArtifactSourceUpdateException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(message: String) = this(message, null)
}

object ArtifactSourceUpdateException {

  def apply(message: String) = new ArtifactSourceUpdateException(message)
}

case class ArtifactSourceCreationException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(message: String) = this(message, null)
}

object ArtifactSourceCreationException {

  def apply(message: String) = new ArtifactSourceCreationException(message)
}