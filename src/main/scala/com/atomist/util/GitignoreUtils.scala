package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file._

import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object GitignoreUtils {

  private val GitignoreFile = ".gitignore"

  def ignoredFiles(path: String, gitignoreFilePaths: List[String] = Nil): List[File] = {
    val pathMatchers = createPathMatchers(path, gitignoreFilePaths)
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

    traverse(Paths.get(path))
    files.toList
  }

  private def createPathMatchers(path: String, gitignoreFilePaths: List[String]): List[PathMatcher] = {
    val fs = FileSystems.getDefault
    val gitignoreFiles = (path :: gitignoreFilePaths).map(p => new File(s"$p/$GitignoreFile"))
    val pathMatchers = ListBuffer.empty[PathMatcher]
    pathMatchers += fs.getPathMatcher(s"glob:**/.git")

    gitignoreFiles.filter(_.exists()).foreach(file => {
      FileUtils.readLines(file, Charset.defaultCharset()).asScala
        .filter(line => !(line.startsWith("#") || line.isEmpty))
        .map(line => {
          val pattern = if (line.endsWith(File.separator)) line.dropRight(1) else line
          pathMatchers += fs.getPathMatcher(s"glob:**/$pattern")
        })
    })
    pathMatchers.toList.distinct
  }
}
