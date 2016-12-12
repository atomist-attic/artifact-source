package com.atomist.util

import java.nio.file.attribute.PosixFilePermission

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

class FilePermissionsTest extends FlatSpec with Matchers {

  it should "convert octal 100755 to decimal" in {
    val permissions = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.OWNER_EXECUTE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.GROUP_EXECUTE,
      PosixFilePermission.OTHERS_READ,
      PosixFilePermission.OTHERS_EXECUTE).asJava

    FilePermissions.toMode(permissions) should equal(33261)
  }

  it should "convert octal 100644 to decimal" in {
    val permissions = Set(
      PosixFilePermission.OWNER_READ,
      PosixFilePermission.OWNER_WRITE,
      PosixFilePermission.GROUP_READ,
      PosixFilePermission.OTHERS_READ).asJava

    FilePermissions.toMode(permissions) should equal(33188)
  }
}
