package com.atomist.util

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class PathUtilsTest extends FlatSpec with Matchers {

  it should "convert Unix-style path to platform-independent path" in {
    val path = "src/main/java/Hello.java"
    val convertedPath = PathUtils.convertPath(path)
    convertedPath should equal(s"src${File.separator}main${File.separator}java${File.separator}Hello.java")
  }
}
