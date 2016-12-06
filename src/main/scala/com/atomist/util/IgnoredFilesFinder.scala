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
    val pathMatchers = createPathMatchers(rootPath)
    val files = ListBuffer.empty[File]
    Files.walkFileTree(Paths.get(rootPath), new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (pathMatchers.exists(_.matches(file))) files += file.toFile
        FileVisitResult.CONTINUE
      }
    })
    files.toList
  }

  private def createPathMatchers(path: String): List[PathMatcher] = {
    val fs = FileSystems.getDefault
    val matcher = fs.getPathMatcher(s"glob:**/$AtomistIgnoreFile")
    val paths = if (dotAtomistignoreFilesExists(path, matcher)) {
      getIgnoredPaths(path, matcher) ++ PathsIgnoredByDefault
    } else {
      val gitignoreFileMatcher = fs.getPathMatcher(s"glob:**/$GitignoreFile")
      val gitignorePaths = getIgnoredPaths(path, gitignoreFileMatcher)
      gitignorePaths ++ getIgnoredPaths(GlobalGitignorePath, gitignoreFileMatcher)
    }
    paths.distinct.map(p => fs.getPathMatcher(s"glob:**/$p"))
  }

  private def getIgnoredPaths(path: String, ignoreFileMatcher: PathMatcher): List[String] = {
    val start = Paths.get(path)
    if (Files.exists(start)) {
      val paths = ListBuffer.empty[String]
      Files.walkFileTree(start, new SimpleFileVisitor[Path]() {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (ignoreFileMatcher.matches(file)) {
            FileUtils.readLines(file.toFile, Charset.defaultCharset()).asScala
              .filterNot(_.startsWith("#"))
              .filterNot(_.isEmpty)
              .map(line => paths += (if (line.endsWith("/") || line.endsWith("\\")) line.dropRight(1) else line))
          }
          FileVisitResult.CONTINUE
        }
      })
      paths.toList
    } else Nil
  }

  private def dotAtomistignoreFilesExists(path: String, matcher: PathMatcher) = {
    val files = ListBuffer.empty[Path]
    Files.walkFileTree(Paths.get(path), new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (matcher.matches(file)) files += file
        FileVisitResult.CONTINUE
      }
    })
    files.nonEmpty
  }
}
