package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class FileArtifactTest extends FlatSpec with Matchers {

  it should "change path of StringFileArtifact" in {
    val sfa = StringFileArtifact("path", "this is the content")
    val newPath = "foo/bar"
    sfa.withPath(newPath) match {
      case updated: StringFileArtifact =>
        updated.path should equal (newPath)
        updated.content should equal (sfa.content)
    }
  }

  it should "change path of ByteArrayFileArtifact" in {
    val sfa = StringFileArtifact("path", "this is the content")
    val newPath = "foo/bar"
    ByteArrayFileArtifact.toByteArrayFileArtifact(sfa).withPath(newPath) match {
      case updated: ByteArrayFileArtifact =>
        updated.path should equal (newPath)
        updated.content should equal (sfa.content)
    }
  }

  it should "change mode of ByteArrayFileArtifact" in {
    val sfa = StringFileArtifact("path", "this is the content")
    val newMode = 66
    ByteArrayFileArtifact.toByteArrayFileArtifact(sfa).withMode(newMode) match {
      case updated: ByteArrayFileArtifact =>
        updated.path should equal (sfa.path)
        updated.pathElements should equal (sfa.pathElements)
        updated.mode should equal (newMode)
        updated.content should equal (sfa.content)
    }
  }

  it should "change id of StringFileArtifact" in {
    val sfa = StringFileArtifact("path", "this is the content")
    val newId = "werwerqewrweqtwe"
    sfa.withUniqueId(newId) match {
      case updated: StringFileArtifact =>
        updated.path should equal (sfa.path)
        updated.pathElements should equal (sfa.pathElements)
        updated.uniqueId should equal (Some(newId))
        updated.content should equal (sfa.content)
    }
  }

  it should "change id of ByteArrayFileArtifact" in {
    val sfa = StringFileArtifact("path", "this is the content")
    val newId = "werwerqewrweqtwe"
    ByteArrayFileArtifact.toByteArrayFileArtifact(sfa).withUniqueId(newId) match {
      case updated: ByteArrayFileArtifact =>
        updated.path should equal (sfa.path)
        updated.pathElements should equal (sfa.pathElements)
        updated.uniqueId should equal (Some(newId))
        updated.content should equal (sfa.content)
    }
  }
}
