package com.atomist.source

abstract class ArtifactSourceException(message: String, cause: Throwable)
  extends Exception(message, cause) {

  protected var statusCode: Int = _

  // For Java callers
  def getStatusCode = statusCode
}

case class ArtifactSourceAccessException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(statusCode: Int, message: String) = {
    this(message, null)
    this.statusCode = statusCode
  }

  def this(message: String) = this(message, null)
}

object ArtifactSourceAccessException {

  def apply(statusCode: Int, message: String) = new ArtifactSourceAccessException(statusCode, message)

  def apply(message: String) = new ArtifactSourceAccessException(message)
}

case class ArtifactSourceUpdateException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(statusCode: Int, message: String) = {
    this(message, null)
    this.statusCode = statusCode
  }

  def this(message: String) = this(message, null)
}

object ArtifactSourceUpdateException {

  def apply(statusCode: Int, message: String) = new ArtifactSourceUpdateException(statusCode, message)

  def apply(message: String) = new ArtifactSourceUpdateException(message)
}

case class ArtifactSourceCreationException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause) {

  def this(statusCode: Int, message: String) = {
    this(message, null)
    this.statusCode = statusCode
  }

  def this(message: String) = this(message, null)
}

object ArtifactSourceCreationException {

  def apply(statusCode: Int, message: String) = new ArtifactSourceCreationException(statusCode, message)

  def apply(message: String) = new ArtifactSourceCreationException(message)
}
