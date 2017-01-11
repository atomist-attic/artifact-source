package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import com.atomist.source.PathFilter
import org.apache.commons.io.FileUtils
import org.springframework.util.AntPathMatcher

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object GitignorePathFilter {

  private val GitIgnoreFile = ".gitignore"

  def apply(rootPath: String) = new GitignorePathFilter(rootPath)
}

class GitignorePathFilter(val rootPath: String) extends PathFilter {

  import GitignorePathFilter._

  private val matchedFiles = {
    val file = Paths.get(rootPath, GitIgnoreFile).toFile
    if (file.exists) {
      val patterns = getPatterns(file)
      val pathMatcher = new AntPathMatcher()
      val files = ArrayBuffer.empty[File]
      files += Paths.get(rootPath, ".git").toFile
      Files.walkFileTree(Paths.get(rootPath), new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
          patterns.foreach(p => if (pathMatcher.`match`(p, dir.toString)) files += dir.toFile)
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes) = {
          patterns.foreach(p => if (pathMatcher.`match`(p, file.toString)) files += file.toFile)
          FileVisitResult.CONTINUE
        }
      })
      files.toList
    } else Nil
  }

  override def apply(path: String): Boolean = {
    val file = new File(path)
    !matchedFiles.exists(f => f.equals(file) || isSubDirectory(f, file))
  }

  def isSubDirectory(base: File, child: File): Boolean = {
    val baseFile = base.getCanonicalFile
    val childFile = child.getCanonicalFile
    var parentFile = childFile
    while (parentFile != null) {
      if (baseFile.equals(parentFile)) {
        return true
      }
      parentFile = parentFile.getParentFile
    }
    false
  }

  private def getPatterns(file: File): List[String] = {
    FileUtils.readLines(file, Charset.defaultCharset()).asScala
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .map(l => if (l.endsWith("/") || l.endsWith("\\")) l.dropRight(1) else l)
      .map(Paths.get(rootPath, _).toString)
      .distinct
      .toList
  }
}
