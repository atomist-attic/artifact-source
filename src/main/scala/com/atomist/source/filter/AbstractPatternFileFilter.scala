package com.atomist.source.filter

import java.io.File
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

abstract class AbstractPatternFileFilter(val rootPath: String) extends ArtifactFilter {

  private val matchedFiles = {
    val file = Paths.get(rootPath, filePath).toFile
    if (file.exists) {
      val patterns = getPatterns(file)
      // val pathMatcher = new AntPathMatcher() // spring-core required
      val fs = FileSystems.getDefault
      val pathMatchers = patterns.map(p =>
        Try(fs.getPathMatcher(s"glob:$p")) match {
          case Success(matcher) => matcher
          case Failure(e) => null
        }
      ) ++ patterns.map(p =>
        Try(fs.getPathMatcher(s"regex:$p")) match {
          case Success(matcher) => matcher
          case Failure(e) => null
        }
      ).filter(_ != null)

      val files = ArrayBuffer.empty[String]
      Files.walkFileTree(Paths.get(rootPath), new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
          if (pathMatchers.exists(_.matches(dir))) files += dir.toString
          // if (patterns.exists(p => pathMatcher.`match`(p, dir.toString))) files += dir.toString
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes) = {
          if (pathMatchers.exists(_.matches(file))) files += file.toString
          // if (patterns.exists(p => pathMatcher.`match`(p, file.toString))) files += file.toString
          FileVisitResult.CONTINUE
        }
      })
      files.distinct.toList
    } else Nil
  }

  protected def filePath: String

  override def apply(path: String): Boolean = !matchedFiles.exists(_.equals(path))

  private def getPatterns(file: File): List[String] = {
    FileUtils.readLines(file, Charset.defaultCharset()).asScala
      .filterNot(l => l.isEmpty || l.startsWith("#"))
      .map(l => if (l.endsWith("/") || l.endsWith("\\")) l.dropRight(1) else l)
      .map(Paths.get(rootPath, _).toString)
      .distinct
      .toList
  }
}
