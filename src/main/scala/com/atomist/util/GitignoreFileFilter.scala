package com.atomist.util

import java.nio.file.Paths

import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.{FileTreeIterator, TreeWalk, WorkingTreeIterator}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case class GitignoreFileFilter(rootPath: String) {

  private val ignoredFiles = {
    val paths = ListBuffer.empty[String]
    Try {
      val builder = new FileRepositoryBuilder
      val repository = builder.readEnvironment().findGitDir().build()
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

  println(ignoredFiles.mkString("\n"))

  def apply(path: String): Boolean = {
    println(s"^^^ $path: matched: ${ignoredFiles.exists(_.equals(path))}")
    !ignoredFiles.exists(_.equals(path))
  }
}
