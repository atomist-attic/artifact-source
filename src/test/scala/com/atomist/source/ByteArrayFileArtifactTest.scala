package com.atomist.source

import org.scalatest.{FlatSpec, Matchers}

class ByteArrayFileArtifactTest extends FlatSpec with Matchers {

  "ByteArrayFileArtifact" should "correctly parse full constructor" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = "".getBytes
    val mode = 100755
    val sfa = ByteArrayFileArtifact(name, pathElements, content, mode, None)
    sfa.name shouldEqual name
    sfa.pathElements.seq shouldEqual pathElements
  }

  it should "correctly parse full path with apply" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = "".getBytes
    val mode = 100755
    val sfa = ByteArrayFileArtifact(pathName = s"${pathElements.mkString("/")}/$name", content = content)
    sfa.name shouldEqual name
    sfa.pathElements.seq shouldEqual pathElements
    sfa shouldEqual ByteArrayFileArtifact(name, pathElements, content, mode, None)
  }

  it should "not permit null path" in {
    an[IllegalArgumentException] should be thrownBy ByteArrayFileArtifact(null, "contents".getBytes)
  }

  it should "not permit empty path" in {
    an[IllegalArgumentException] should be thrownBy ByteArrayFileArtifact("", "contents".getBytes)
  }

  it should "not permit path with only /" in {
    an[IllegalArgumentException] should be thrownBy ByteArrayFileArtifact("/", "contents".getBytes)
  }

  it should "not permit relative paths" in {
    an[IllegalArgumentException] should be thrownBy ByteArrayFileArtifact("./test.txt", "contents".getBytes)
  }

  it should "not permit paths starting with ../" in {
    an[IllegalArgumentException] should be thrownBy ByteArrayFileArtifact("../test.txt", "contents".getBytes)
  }

  it should "remove opening / from artifacts in root" in {
    val (name, contents) = ("name", "contents")
    val withoutSlash = ByteArrayFileArtifact(name, contents.getBytes)
    val withSlash = ByteArrayFileArtifact("/" + name, contents.getBytes)
    withSlash.path should equal(name)
    withSlash should equal(withoutSlash)
    withSlash.pathElements should be(empty)
    withoutSlash.pathElements should be(empty)
  }

  it should "remove opening / from artifacts in nested path" in {
    val (path, contents) = ("src/main/java/Hello.java", "contents")
    val withoutSlash = ByteArrayFileArtifact(path, contents.getBytes)
    val withSlash = ByteArrayFileArtifact("/" + path, contents.getBytes)
    withSlash.path should equal(path)
    withSlash should equal(withoutSlash)
    withSlash.pathElements.length should equal(3)
    withoutSlash.pathElements.length should equal(3)
  }

  it should "create ByteArrayFileArtifact and modify mode and uniqueId" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = "hello world"
    val mode = 100755
    val uniqueId = Some("atomist")
    val sfa = ByteArrayFileArtifact(name, pathElements, content.getBytes, mode, uniqueId)
    sfa.content should equal(content)
    sfa.mode should equal(mode)
    sfa.uniqueId should equal(uniqueId)

    val sfa1 = sfa
      .withContent("Atomist")
      .withMode(FileArtifact.ExecutableMode)
      .withUniqueId("foobar")
    sfa1.content should equal("Atomist")
    sfa1.mode should equal(FileArtifact.ExecutableMode)
    sfa1.uniqueId.get should equal("foobar")
  }
}
