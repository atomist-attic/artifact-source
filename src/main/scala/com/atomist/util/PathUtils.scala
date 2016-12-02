package com.atomist.util

import java.io.File
import java.util.regex.Matcher

object PathUtils {

  private val IsWindows = System.getProperty("os.name").contains("indows")

  private val fileSeparator = Matcher.quoteReplacement(File.separator)

  def splitPath(path: String): Array[String] = convertPath(path).split(fileSeparator)

  def convertPaths(paths: Seq[String]) = paths.map(convertPath(_))

  def convertPath(path: String) = {
    if (path == null || path.equals("")) path
    else {
      val convertedPath = path.replaceAll("\\\\|/", fileSeparator)
      if (IsWindows) convertedPath.replace(":", "_") else convertedPath
    }
  }
}
