package com.atomist.source

/**
  * Trait extended by classes that can locate an ArtifactSource.
  * This trait indicates where to look: ArtifactSourceIdentifier
  * may provide further information about what was found.
  */
trait ArtifactSourceLocator {

  def name: String
}

/**
  * Uniquely identifies an ArtifactSource. May contain a fingerprint such as a GitHub commit sha.
  */
trait ArtifactSourceIdentifier extends ArtifactSourceLocator {
}

object ArtifactSourceLocator {

  import scala.language.implicitConversions

  implicit def stringToArtifactSourceIdentifier(s: String): StringArtifactSourceLocator = StringArtifactSourceLocator(s)
}

case class StringArtifactSourceLocator(name: String)
  extends ArtifactSourceLocator

case class NamedArtifactSourceIdentifier(name: String)
  extends ArtifactSourceIdentifier

/**
  * Information about a change such as a commit.
  */
trait SourceUpdateInfo {

  def message: String
}

case class SimpleSourceUpdateInfo(message: String) extends SourceUpdateInfo
