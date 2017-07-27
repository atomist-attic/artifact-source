package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceExceptionTest extends FlatSpec with Matchers {

  it should "create new ArtifactSourceAccessException with message" in {
    val e = new ArtifactSourceException("message")
    e.getMessage should equal("message")
    e.getCause should be(null)
  }

  it should "create new ArtifactSourceCreationException with statusCode and message" in {
    val e = new ArtifactSourceException("message")
    e.getMessage should equal("message")
    e.getCause should be(null)
  }

  it should "create new ArtifactSourceUpdateException with message and throwable" in {
    val iae = new IllegalArgumentException("illegal argument")
    val e = new ArtifactSourceException("message", iae)
    e.getMessage should equal("message")
    e.getCause should not be null
    e.getCause.getMessage should equal("illegal argument")
  }
}
