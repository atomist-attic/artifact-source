package com.atomist.util

import java.nio.file.attribute.PosixFilePermission

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

class FilePermissionsTest extends FlatSpec with Matchers {

  "FilePermissions" should "convert octal 100755 to decimal" in {
    val permissions = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.GROUP_EXECUTE,
      PosixFilePermission.OTHERS_READ,
      PosixFilePermission.OTHERS_EXECUTE)

    FilePermissions.toMode(permissions.asJava) should equal(33261)
  }

  it should "convert octal 100644 to decimal" in {
    val permissions = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.OTHERS_READ)

    FilePermissions.toMode(permissions.asJava) should equal(33188)
  }

  it should "convert PosixFilePermissions to int mode" in {
    val expected = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.OTHERS_READ)

    val actual = FilePermissions.fromMode(33188).asScala
    actual should contain theSameElementsAs expected
  }

  it should "convert PosixFilePermissions to int mode for executable files" in {
    val expected = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.GROUP_EXECUTE,
      PosixFilePermission.OTHERS_READ,
      PosixFilePermission.OTHERS_EXECUTE)

    val actual = FilePermissions.fromMode(33261).asScala
    actual should contain theSameElementsAs expected
  }
}
