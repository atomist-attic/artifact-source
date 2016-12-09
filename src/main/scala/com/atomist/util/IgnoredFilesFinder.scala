package com.atomist.util

import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files.isDirectory
import java.nio.file._

import org.apache.commons.io.FileUtils
import org.eclipse.jgit.ignore.internal.PathMatcher
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.{FileTreeIterator, TreeWalk, WorkingTreeIterator}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object IgnoredFilesFinder {

  private val AtomistIgnoreFile = ".ignore"

  private case class MatchedPaths(matched: List[String], pathsToKeep: List[String])

  def ignoredFiles(rootPath: String): List[File] = {
    val paths = ListBuffer.empty[String]
    Try {
      paths ++= handleGitignore(rootPath)
    } match {
      case Success(_) =>
      case Failure(e) =>
    }

    Try {
      handleAtomistIgnore(rootPath)
    } match {
      case Success(matchedPaths) => paths --= matchedPaths.pathsToKeep ++= matchedPaths.matched
      case Failure(e) =>
    }

    paths.distinct.map(new File(_)).toList
  }

  private def handleGitignore(rootPath: String) = {
    val paths = ListBuffer.empty[String]
    Try {
      val builder = new FileRepositoryBuilder
      val repository = builder.setGitDir(new File(rootPath, ".git")).readEnvironment().findGitDir().build()
      val treeWalk = new TreeWalk(repository)
      treeWalk.addTree(new FileTreeIterator(repository))
      while (treeWalk.next()) {
        if (treeWalk.isSubtree)
          treeWalk.enterSubtree()

        val itr = treeWalk.getTree(0, classOf[WorkingTreeIterator])
        if (itr != null && itr.isEntryIgnored)
          paths += Paths.get(rootPath, treeWalk.getPathString).toString
      }
    } match {
      case Success(_) => paths.distinct.toList
      case Failure(e) => Nil
    }
  }

  private def handleAtomistIgnore(rootPath: String) = {
    val path = Paths.get(rootPath, ".atomist")
    val paths = Files.walk(path).iterator().asScala.toList

    val patterns = readIgnoreFile(path)

    val matchers = patterns.map(createPathMatcher(path, _))
    val matched = paths.filter(p => matchers.exists(_.matches(p.toString, isDirectory(p)))).map(_.toString).distinct

    val negatedMatchers = patterns.filter(_.startsWith("!")).map(_.substring(1)).map(createPathMatcher(path, _))
    val pathsToKeep = paths.filter(p => negatedMatchers.exists(_.matches(p.toString, isDirectory(p)))).map(_.toString).distinct

    MatchedPaths(matched, pathsToKeep)
  }

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


