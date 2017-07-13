package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceEditTest extends FlatSpec with Matchers {

  val f1 = StringFileArtifact("Name.java", "my/new/path", "contents")
  val f2 = StringFileArtifact("Name2.scala", "my/other", "contents")
  val test1: ArtifactSource = EmptyArtifactSource() + Seq(f1, f2)

  "ArtifactSource edit" should "make no change when no edits apply" in {
    val nopEditor = SimpleFileEditor(_ => false, f => f)
    val edited = test1 ✎ nopEditor
    edited should be theSameInstanceAs test1
  }

  it should "replace file with new contents" in {
    val newContent = "newContent"
    val javaEditor = SimpleFileEditor(f => f.name.endsWith(".java"), f => StringFileArtifact(f.path, newContent))
    val edited = test1 ✎ javaEditor
    edited should not be theSameInstanceAs(test1)
    test1.findFile("my/new/path/Name.java").get.content should equal("contents")
    edited.findFile("my/new/path/Name.java").get.content should equal(newContent)
  }

  it should "editing existing file contents" in {
    val oldContent = "contents"
    val newContent = "newContent"
    val javaEditor = SimpleFileEditor(f => f.name.endsWith(".java"), f => StringFileArtifact(f.path, f.content + newContent))
    val edited = test1 ✎ javaEditor
    edited should not be theSameInstanceAs(test1)
    test1.findFile("my/new/path/Name.java").get.content should equal(oldContent)
    edited.findFile("my/new/path/Name.java").get.content should equal(oldContent + newContent)
  }
}
