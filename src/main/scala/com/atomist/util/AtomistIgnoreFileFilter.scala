package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files.isDirectory
import java.nio.file._

import org.apache.commons.io.FileUtils
import org.eclipse.jgit.ignore.internal.PathMatcher

import scala.collection.JavaConverters._

object AtomistIgnoreFileFilter {

  private val AtomistIgnoreFile = "ignore"

  def apply(rootPath: String) = new AtomistIgnoreFileFilter(rootPath)
}

class AtomistIgnoreFileFilter(val rootPath: String) {

  import AtomistIgnoreFileFilter._

  private val ignoredFiles = {
    val path = Paths.get(rootPath, ".atomist")
    val paths = Files.walk(path).iterator().asScala.toList

    val patterns = readIgnoreFile(path)

    val matchers = patterns.map(createPathMatcher(path, _))
    val matched = paths.filter(p => matchers.exists(_.matches(p.toString, isDirectory(p)))).map(_.toString).distinct

    val negatedMatchers = patterns.filter(_.startsWith("!")).map(_.substring(1)).map(createPathMatcher(path, _))
    val pathsToKeep = paths.filter(p => negatedMatchers.exists(_.matches(p.toString, isDirectory(p)))).map(_.toString).distinct

    matched diff pathsToKeep
  }

  def apply(path: String): Boolean = ignoredFiles.exists(_.equals(path))

  private def readIgnoreFile(path: Path): List[String] = {
    val file = new File(path.toString, AtomistIgnoreFile)
    FileUtils.readLines(file, Charset.defaultCharset()).asScala
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .map(l => if (l.endsWith("/") || l.endsWith("\\")) l.dropRight(1) else l)
      .distinct
      .toList
  }

  private def createPathMatcher(path: Path, pathToResolve: String) =
    PathMatcher.createPathMatcher(path.resolve(pathToResolve).toString, File.separatorChar, false)
}
