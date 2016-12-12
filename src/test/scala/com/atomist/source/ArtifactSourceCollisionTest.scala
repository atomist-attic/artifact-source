package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceCollisionTest extends FlatSpec with Matchers {

  it should "allow overwrite of file by default" in {
    val f1 = StringFileArtifact("name", "whatever")
    val f2 = f1.copy(_content = "whatever else")

    val before = new SimpleFileBasedArtifactSource("", f1)
    val r = before + f2
    r.totalFileCount should equal(before.totalFileCount)
    before.findFile(f1.path).get should equal(f1)
    r.findFile(f1.path).get should equal(f2)
  }

  it should "allow overwrite of file using ArtifactSource by default" in {
    val f1 = StringFileArtifact("name", "whatever")
    val f2 = f1.copy(_content = "whatever else")

    val before = new SimpleFileBasedArtifactSource("", f1)
    val toAdd = new SimpleFileBasedArtifactSource("", f2)

    val r = before + toAdd
    r.totalFileCount should equal(before.totalFileCount)
    before.findFile(f1.path).get should equal(f1)
    r.findFile(f1.path).get should equal(f2)
  }

  it should "detect file collision" in {
    val f1 = StringFileArtifact("name", "whatever")
    val f2 = f1.copy(_content = "whatever else")
    val f3 = StringFileArtifact("quite/something/else", "content doesn't matter")

    val before = new SimpleFileBasedArtifactSource("", f1)
    val toAdd = new SimpleFileBasedArtifactSource("", Seq(f2, f3))

    val r = before + toAdd
    val deltas = r Δ before
    deltas.deltas.size should be >= 2
    deltas.collisions.size should equal(1)
    val delta = deltas.collisions.head
    delta.path should equal(f1.path)
  }

  it should "detect file collision in different artifact sources" in {
    val f1 = StringFileArtifact("name", "whatever")
    val f2 = f1.copy(_content = "whatever else")
    val f3 = StringFileArtifact("quite/something/else", "content doesn't matter")

    val before = new SimpleFileBasedArtifactSource("as1", f1)
    val toAdd = new SimpleFileBasedArtifactSource("as2", Seq(f2, f3))

    val r = before + toAdd

    val deltas = r Δ before
    deltas.deltas.size should be >= 2
    deltas.collisions.size should equal(1)
    val delta = deltas.collisions.head
    delta.path should equal(f1.path)

    val deltas2 = r Δ toAdd
    deltas2.deltas.size should equal(0)
    deltas2.collisions.size should equal(1)
    val delta2 = deltas2.collisions.head
    delta2.path should equal(f1.path)
  }

  it should "detect no file collisions" in {
    val f1 = StringFileArtifact("foo", "whatever1")
    val f2 = StringFileArtifact("bar", "whatever2")
    val f3 = StringFileArtifact("quite/something/else", "content doesn't matter")

    val before = new SimpleFileBasedArtifactSource("", f1)
    val toAdd = new SimpleFileBasedArtifactSource("", Seq(f2, f3))

    val r = before + toAdd

    val deltas = r Δ before
    deltas.deltas.size should be >= 2
    deltas.collisions.size should equal(0)

    val deltas2 = r Δ toAdd
    deltas2.deltas.size should be >= 2
    deltas2.collisions.size should equal(0)
  }

  it should "detect no file collisions in different artifact sources" in {
    val f1 = StringFileArtifact("foo", "whatever1")
    val f2 = StringFileArtifact("bar", "whatever2")
    val f3 = StringFileArtifact("quite/something/else", "content doesn't matter")

    val before = new SimpleFileBasedArtifactSource("as1", f1)
    val toAdd = new SimpleFileBasedArtifactSource("as2", Seq(f2, f3))

    val r = before + toAdd

    val deltas = r Δ before
    deltas.deltas.size should be >= 2
    deltas.collisions.size should equal(0)

    val deltas2 = r Δ toAdd
    deltas2.deltas.size should equal(1)
    deltas2.collisions.size should equal(0)
  }

  // Is the right behavior to reject it?
  it should "handle trying to add a file in place of a directory" is pending

  it should "handle trying to add a directory in place of a file" is pending
}
