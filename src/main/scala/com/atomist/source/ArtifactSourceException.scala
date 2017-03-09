package com.atomist.source

abstract class ArtifactSourceException(message: String, cause: Throwable)
  extends Exception(message, cause)

case class ArtifactSourceAccessException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause)

case class ArtifactSourceUpdateException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause)

case class ArtifactSourceCreationException(message: String, cause: Throwable)
  extends ArtifactSourceException(message, cause)