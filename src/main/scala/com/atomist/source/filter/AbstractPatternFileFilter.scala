package com.atomist.source.filter

import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

abstract class AbstractPatternFileFilter(val rootPath: String) extends ArtifactFilter {

  override def apply(path: String): Boolean =
    !(matched._1.exists(Paths.get(path).toAbsolutePath.toString.startsWith(_)) || matched._2.exists(_.equals(path)))

  protected def filePath: String

  private lazy val matched: (List[String], List[String]) = {
    val file = Paths.get(rootPath, filePath).toFile
    if (file.exists) {
      val patterns = FileUtils.readLines(file, Charset.defaultCharset()).asScala
        .filterNot(l => l.isEmpty || l.startsWith("#"))
        .map(l => if (l.endsWith("/") || l.endsWith("\\")) l.dropRight(1) else l)
        .distinct
        .toList

      // val pathMatcher = new AntPathMatcher() // spring-core required
      val fs = FileSystems.getDefault
      val pathMatchers = patterns.map(p =>
        Try(fs.getPathMatcher(s"glob:**/$p")) match {
          case Success(matcher) => matcher
          case Failure(_) => null
        }
      ) ++ patterns.map(p =>
        Try(fs.getPathMatcher(s"regex:**/$p")) match {
          case Success(matcher) => matcher
          case Failure(_) => null
        }
      ).filter(_ != null)

      val files = ArrayBuffer.empty[String]
      val dirs = ArrayBuffer.empty[String]
      Files.walkFileTree(Paths.get(rootPath), new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
          // if (patterns.exists(p => pathMatcher.`match`(p, dir.toString))) {
          if (pathMatchers.exists(_.matches(dir))) {
            dirs += dir.toString
            FileVisitResult.SKIP_SUBTREE
          } else FileVisitResult.CONTINUE
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes) = {
          // if (patterns.exists(p => pathMatcher.`match`(p, file.toString))) files += file.toString
          if (pathMatchers.exists(_.matches(file))) files += file.toString
          FileVisitResult.CONTINUE
        }
      })
      (dirs.toList, files.toList)
    } else (Nil, Nil)
  }
}