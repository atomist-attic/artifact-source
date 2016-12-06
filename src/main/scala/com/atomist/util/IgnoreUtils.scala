package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object IgnoreUtils {

  private val GlobalGitignorePath = s"${System.getProperty("user.home")}/.config/git/ignore"
  private val GitignoreFile = ".gitignore"
  private val AtomistIgnoreFile = ".atomistignore"
  private val PathsIgnoredByDefault = List(".git", ".svn", "*.DS_Store", "*.iml", ".idea", ".project", ".classpath", ".settings")

  def ignoredFiles(rootPath: String): List[File] = {
    val pathMatchers = createPathMatchers(rootPath)
    val files = ListBuffer.empty[File]

    def traverse(path: Path) {
      if (Files.isDirectory(path)) {
        path.toFile.listFiles().foreach(child => {
          if (pathMatchers.exists(m => m.matches(child.toPath))) files += child
          if (child.isDirectory) traverse(child.toPath)
        })
      } else if (pathMatchers.exists(m => m.matches(path))) {
        files += path.toFile
      }
    }

    traverse(Paths.get(rootPath))
    files.toList
  }

  private def createPathMatchers(path: String): List[PathMatcher] = {
    val fs = FileSystems.getDefault
    (getIgnoredPaths(path, fs.getPathMatcher(s"glob:**/$AtomistIgnoreFile")) match {
      case Nil =>
        val gitignoreFileMatcher = fs.getPathMatcher(s"glob:**/$GitignoreFile")
        val gitignorePaths = getIgnoredPaths(path, gitignoreFileMatcher)
        gitignorePaths ++ getIgnoredPaths(GlobalGitignorePath, gitignoreFileMatcher)
      case other => other ++ PathsIgnoredByDefault
    }).distinct.map(p => fs.getPathMatcher(s"glob:**/$p"))
  }

  private def getIgnoredPaths(path: String, ignoreFileMatcher: PathMatcher) = {
    val paths = ListBuffer.empty[String]
    Files.walkFileTree(Paths.get(path), new SimpleFileVisitor[Path]() {
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
  }
}
