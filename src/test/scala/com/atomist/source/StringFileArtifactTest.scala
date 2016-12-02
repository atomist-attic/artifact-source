package com.atomist.source

import java.io.File
import java.nio.file.Paths

import com.atomist.util.Utils
import org.scalatest.{FlatSpec, Matchers}

class StringFileArtifactTest extends FlatSpec with Matchers {

  "StringFileArtifact" should "correctly parse full constructor" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = ""
    val mode = 100755
    val sfa = StringFileArtifact(name, pathElements, content, mode, None)
    sfa.name shouldEqual name
    sfa.pathElements.seq shouldEqual pathElements
  }

  it should "correctly parse full path with apply" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = ""
    val mode = 100755
    val sfa = StringFileArtifact(pathName = s"${pathElements.mkString(File.separator)}/$name", content = content)
    sfa.name shouldEqual name
    sfa.pathElements.seq shouldEqual pathElements
    sfa shouldEqual StringFileArtifact(name, pathElements, content, mode, None)
  }

  it should "not permit null path" in {
    an[IllegalArgumentException] should be thrownBy StringFileArtifact(null, "contents")
  }

  it should "not permit empty path" in {
    an[IllegalArgumentException] should be thrownBy StringFileArtifact("", "contents")
  }

  it should "not permit path with only /" in {
    an[IllegalArgumentException] should be thrownBy StringFileArtifact("/", "contents")
  }

  it should "remove opening / from artifacts in root" in {
    val (name, contents) = ("name", "contents")
    val withoutSlash = StringFileArtifact(name, contents)
    val withSlash = StringFileArtifact("/" + name, contents)
    withSlash.path should equal(name)
    withSlash should equal(withoutSlash)
    withSlash.pathElements should be(empty)
    withoutSlash.pathElements should be(empty)
  }

  it should "remove opening / from artifacts in nested path" in {
    val (path, contents) = (Paths.get("src", "main", "java", "Hello.java").toString, "contents")
    val withoutSlash = StringFileArtifact(path, contents)
    val withSlash = StringFileArtifact("/" + path, contents)
    withSlash.path should equal(path)
    withSlash should equal(withoutSlash)
    withSlash.pathElements.length should equal(3)
    withoutSlash.pathElements.length should equal(3)
  }

  it should "create StringFileArtifact and modify mode and uniqueId" in {
    val name = "filename"
    val pathElements = Seq("com", "atomist")
    val content = "hello world"
    val mode = 100755
    val uniqueId = Some("atomist")
    val sfa = StringFileArtifact(name, pathElements, content, mode, uniqueId)
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
