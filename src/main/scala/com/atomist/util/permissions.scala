package com.atomist.util

import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.util.{Set => JSet}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object FilePermissions {

  def toMode(permissions: JSet[PosixFilePermission]): Int = {
    var mode = 0
    permissions.asScala.foreach(p => {
      if (p == PosixFilePermission.OWNER_READ) mode |= octalToInt("0400")
      if (p == PosixFilePermission.OWNER_WRITE) mode |= octalToInt("0200")
      if (p == PosixFilePermission.OWNER_EXECUTE) mode |= octalToInt("0100")
      if (p == PosixFilePermission.GROUP_READ) mode |= octalToInt("0040")
      if (p == PosixFilePermission.GROUP_WRITE) mode |= octalToInt("0020")
      if (p == PosixFilePermission.GROUP_EXECUTE) mode |= octalToInt("0010")
      if (p == PosixFilePermission.OTHERS_READ) mode |= octalToInt("0004")
      if (p == PosixFilePermission.OTHERS_WRITE) mode |= octalToInt("0002")
      if (p == PosixFilePermission.OTHERS_EXECUTE) mode |= octalToInt("0001")
      else mode |= 0
    })
    mode
  }

  def intToOctal(i: Int): String = {
    def convertToOctal(n: Int, acc: String): String =
      if (n < 8) (acc + n).reverse
      else convertToOctal(n / 8, acc + (n % 8))

    convertToOctal(i, "")
  }

  def octalToInt(s: String): Int =
    Try(Integer.parseInt(s, 8)) match {
      case Success(i) => i
      case Failure(e) => throw new IllegalArgumentException(e)
    }
}

object Permissions {

  def apply(perms: String): java.util.Set[PosixFilePermission] =
    PosixFilePermissions fromString convert(perms)

  def convert(perms: String) = {
    require(perms.length == 4 || perms.length == 3, s"Permissions must have 3 or 4 digits, got [$perms]")
    // Ignore setuid/setguid/sticky bit
    val i = if (perms.length == 3) 0 else 1
    val user = Character getNumericValue (perms charAt i)
    val group = Character getNumericValue (perms charAt i + 1)
    val other = Character getNumericValue (perms charAt i + 2)
    asString(user) + asString(group) + asString(other)
  }

  private def asString(perm: Int) = perm match {
    case 0 => "---"
    case 1 => "--x"
    case 2 => "-w-"
    case 3 => "-wx"
    case 4 => "r--"
    case 5 => "r-x"
    case 6 => "rw-"
    case 7 => "rwx"
  }
}
