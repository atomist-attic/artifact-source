package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object IgnoredFilesFinder {

  private val GlobalGitignorePath = s"${System.getProperty("user.home")}/.config/git/ignore"
  private val GitignoreFile = ".gitignore"
  private val AtomistIgnoreFile = ".atomistignore"
  private val PathsIgnoredByDefault = List(".git", ".svn", "*.DS_Store", "*.iml", ".idea", ".project", ".classpath", ".settings")

  def ignoredFiles(rootPath: String): List[File] = {
    val ignoredFiles = (file: Path) =>
      if (createPathMatchers(rootPath).exists(_.matches(file))) List(file.toString) else List.empty

    walkTree(rootPath, ignoredFiles)
      .filterNot(_.contains(AtomistIgnoreFile))
      .filterNot(_.contains(GitignoreFile))
      .map(f => new File(f))
  }

  private def createPathMatchers(path: String): List[PathMatcher] = {
    val fs = FileSystems.getDefault
    val matcher = fs.getPathMatcher(s"glob:**/$AtomistIgnoreFile")
    val matchedPaths = (file: Path) => if (matcher.matches(file)) List(file.toString) else List.empty
    val paths = walkTree(path, matchedPaths) match {
      case paths if paths.nonEmpty => getIgnoredPaths(path, matcher) ++ PathsIgnoredByDefault
      case _ =>
        val gitignoreFileMatcher = fs.getPathMatcher(s"glob:**/$GitignoreFile")
        val gitignorePaths = getIgnoredPaths(path, gitignoreFileMatcher)
        gitignorePaths ++ getIgnoredPaths(GlobalGitignorePath, gitignoreFileMatcher)
    }
    paths.distinct.map(p => fs.getPathMatcher(s"glob:**/$p"))
  }

  private def getIgnoredPaths(path: String, matcher: PathMatcher): List[String] = {
    val start = Paths.get(path)
    if (Files.exists(start)) {
      val ignoredPaths = (file: Path) =>
        if (matcher.matches(file))
          FileUtils.readLines(file.toFile, Charset.defaultCharset()).asScala
            .filterNot(_.startsWith("#"))
            .filterNot(_.isEmpty)
            .map(line => if (line.endsWith("/") || line.endsWith("\\")) line.dropRight(1) else line)
            .toList
        else
          List.empty

      walkTree(path, ignoredPaths)
    } else
      Nil
  }

  private def walkTree(path: String, processFile: Path => List[String]) = {
    val paths = ListBuffer.empty[String]
    val file = Paths.get(path)
    Files.walkFileTree(file, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        paths ++= processFile(file)
        FileVisitResult.CONTINUE
      }
    })
    paths.distinct.toList
  }
}
