package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ArtifactSourceFilterTest extends FlatSpec with Matchers {

  it should "filter file outside root" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", Nil, "contents")
    val f3 = StringFileArtifact("name", "my/other/path", "contents")
    val result = as plus Seq(f1, f2, f3)

    result.allFiles.size should equal(3)
    result.allDirectories.size should equal(3)
    result.findFile("name") shouldBe defined
    val filtered = result.filter(_ => true, f => !f.name.equals("name"))
    filtered.allFiles.size should equal(1)
    filtered.allDirectories.map(_.path).sorted should equal(Seq("my", "my/other", "my/other/path").sorted)
  }

  it should "filter file in root" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", "my/other/path", "contents")
    val result = as plus Seq(f1, f2)

    result.allFiles.size should equal(2)
    result.allDirectories.size should equal(3)
    result.findFile("name") shouldBe defined
    val filtered = result.filter(_ => true, f => !f.name.equals("name"))
    filtered.allFiles.size should equal(1)
    filtered.allDirectories.size should equal(3)
  }

  it should "filter directory outside root" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", "my/new", "contents")
    val f3 = StringFileArtifact("name", "my/new/path", "contents")
    val result = as plus Seq(f1, f2, f3)

    result.directories.map(_.path).toSet should equal(Set("my"))
    result.allDirectories.map(_.path).seq.sorted should equal(Seq("my", "my/new", "my/new/path").sorted)

    result.allFiles.size should equal(3)
    result.allDirectories.size should equal(3)
    result.findFile("name") shouldBe defined
    val filtered = result.filter(d => !d.path.equals("my/new"), _ => true)
    filtered.allFiles.size should equal(1)
    filtered.allDirectories.size should equal(1)
  }

  it should "delete file outside root" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", "my/other/path", "contents")
    val result = as plus Seq(f1, f2)

    result.findFile("my/other/path/name2") shouldBe defined
    result.allFiles.size should equal(2)
    result.allDirectories.size should equal(3)
    val filtered = result - "my/other/path/name2"
    filtered.allFiles.size should equal(1)
    filtered.allDirectories.size should equal(3)
    filtered.findFile("my/other/path/name2") should not be defined
  }

  it should "delete file in root" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", Nil, "contents")
    val f3 = StringFileArtifact("name", "my/other/path", "contents")
    val result = as plus Seq(f1, f2, f3)

    result.findFile("name") shouldBe defined
    result.allFiles.size should equal(3)
    result.allDirectories.size should equal(3)
    result.findFile("name") shouldBe defined
    val filtered = result - "name"
    filtered.allFiles.size should equal(2)
    filtered.allDirectories.size should equal(3)
    filtered.findFile("name") should not be defined
  }

  it should "remove empty directories" in {
    val as = EmptyArtifactSource("erer")
    val f1 = StringFileArtifact("name", Nil, "contents")
    val f2 = StringFileArtifact("name2", "my/new", "contents")
    val f3 = StringFileArtifact("name2", "my/new/path", "contents")
    val f4 = StringFileArtifact("notname2", "my/other/path", "contents")
    val result = as plus Seq(f1, f2, f3, f4)

    result.directories.map(_.path).toSet should equal(Set("my"))

    result.allFiles.size should equal(4)
    result.allDirectories.size should equal(5)
    result.findFile("name") shouldBe defined
    val filtered = result.filter(_ => true, f => !f.name.equals("name2")).removeEmptyDirectories()
    filtered.allFiles.size should equal(2)
    filtered.allDirectories.foreach(_.empty shouldBe false)
    filtered.allDirectories.size should equal(3)
  }
}
