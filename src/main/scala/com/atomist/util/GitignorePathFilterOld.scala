package com.atomist.util

import java.nio.file.Paths

import com.atomist.source.PathFilter
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.{FileTreeIterator, TreeWalk, WorkingTreeIterator}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

case class GitignorePathFilterOld(rootPath: String) extends PathFilter {

  private val ignoredFiles = {
    val paths = ListBuffer.empty[String]
    Try {
      val gitDir = Paths.get(rootPath, ".git").toFile
      if (gitDir.exists())
        paths += gitDir.getPath

      val git = Git.open(gitDir)
      val repository = git.getRepository
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

  // println(ignoredFiles.mkString("\n"))

  override def apply(path: String): Boolean = ignoredFiles.exists(_.startsWith(path))
}
