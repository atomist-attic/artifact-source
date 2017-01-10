package com.atomist.source

import java.util.function.{Function => JFunction}

import com.atomist.source.ArtifactSource.FileFilter

import scala.collection.mutable.ListBuffer
import scala.language.{implicitConversions, postfixOps}

trait FileEditor {

  def edit(f: FileArtifact): FileArtifact

  def canAffect(f: FileArtifact): Boolean
}

case class SimpleFileEditor(file: FileArtifact => Boolean, f: FileArtifact => FileArtifact) extends FileEditor {

  override def edit(fa: FileArtifact): FileArtifact = f(fa)

  override def canAffect(fa: FileArtifact): Boolean = file(fa)
}

object ArtifactSource {

  type ArtifactDirFilter = String => Boolean

  type ArtifactFileFilter = String => Boolean

  type DirFilter = DirectoryArtifact => Boolean

  type FileFilter = FileArtifact => Boolean

  type ArtifactSourceSplit = (ArtifactSource, ArtifactSource)

  implicit def enhancedFileFilter(ff: FileFilter): EnhancedFileFilter = new EnhancedFileFilter(ff)

  /**
    * Change paths inside this ArtifactSource.
    *
    * @param newId new id
    * @param allFiles artifacts (potentially with children) to include
    * @param newPathFor newPathFor
    * @return an ArtifactSource
    */
  def repathed(newId: String, allFiles: Seq[FileArtifact], newPathFor: Artifact => Seq[String]): ArtifactSource = {
    val deltas = allFiles.map(oldFile =>
      FileUpdateDelta(oldFile, ByteArrayFileArtifact.repathed(oldFile, newPathFor(oldFile)))
    )
    new SimpleFileBasedArtifactSource(NamedArtifactSourceIdentifier(newId), deltas.map(_.updatedFile), deltas)
  }

  def of(pathsAndContents: Map[String, String]) = pathsAndContents.map(pac => StringFileArtifact(pac._1, pac._2)).toList

  def fromFiles(fileArtifacts: FileArtifact*) =
    new SimpleFileBasedArtifactSource(fileArtifacts.hashCode.toString, fileArtifacts)
}

class EnhancedFileFilter(ff: FileFilter) extends FileFilter {

  override def apply(f: FileArtifact): Boolean = ff(f)

  def unary_! = new EnhancedFileFilter(f => !ff.apply(f))

  def ||(f2: FileFilter): FileFilter = f => ff(f) || f2(f)
}

import com.atomist.source.ArtifactSource._

/**
  * Criteria determining how an ArtifactSource should be split.
  *
  * @param shared predicate for files that should be shared
  * @param toA predicates for files that should go into left artifact source
  */
case class SplitCriteria(shared: FileFilter, toA: FileFilter)

/**
  * Immutable source of artifacts. Might be sourced from a repository or file system, for example.
  * Used to source code to introspect, expose the results of code generation,
  * or hold code generation templates (thereby transparently offering version control).
  */
trait ArtifactSource extends RootArtifactContainer {

  val id: ArtifactSourceIdentifier

  def cachedDeltas: Seq[Delta] = Nil

  def collisions: Seq[Delta] = cachedDeltas collect {
    case fud: FileUpdateDelta => fud
  }

  def getIdString: String = id.toString

  /**
    * Timestamp of latest change to these artifacts.
    *
    * @return a timestamp
    */
  def lastModified: Long = throw new UnsupportedOperationException

  def plus(right: ArtifactSource): ArtifactSource =
    new ArtifactSource {
      private val left = ArtifactSource.this

      override val id: ArtifactSourceIdentifier = left.id

      override lazy val cachedDeltas: Seq[Delta] =
        (for (newFile <- right.allFiles)
          yield
            left.findFile(newFile.path) match {
              case Some(oldFile) => FileUpdateDelta(oldFile, newFile)
              case None => FileAdditionDelta(newFile)
            }) ++
          (for (newDir <- right.allDirectories)
            yield
              left.findDirectory(newDir.path) match {
                case Some(d) => None
                case None => Some(DirectoryAdditionDelta(newDir))
              }
            ).flatten

      override def allDirectories: Seq[DirectoryArtifact] =
        right.allDirectories ++ left.allDirectories.filter(f => !right.allDirectories.exists(_.path.equals(f.path)))

      override def allFiles: Seq[FileArtifact] =
        right.allFiles ++ left.allFiles.filter(f => !right.allFiles.exists(_.path.equals(f.path)))

      override def artifacts: Seq[Artifact] = allDirectories ++ allFiles
    }

  def plus(newArtifact: Artifact): ArtifactSource =
  // TODO what to do in event of conflict
    new ArtifactSource with DirectoryBasedArtifactContainer {
      override val id: ArtifactSourceIdentifier = ArtifactSource.this.id

      override val artifacts: Seq[Artifact] = {
        def replaceOldWithNew(arts: Seq[Artifact], theNewArtifact: Artifact) = (arts collect {
          case art if !(art.name equals theNewArtifact.name) => art
        }) :+ theNewArtifact

        def findOrCreateDirectory(pathAbove: Seq[String], pathBelow: Seq[String], parent: ArtifactContainer): Artifact = {
          if (newArtifact match {
            case d: DirectoryArtifact if pathBelow.equals(Seq(newArtifact.name)) => true
            case f: FileArtifact if pathBelow.isEmpty => true
            case _ => false
          })
            newArtifact
          else {
            val theRightDirectory = parent.findChildDirectory(pathBelow.head) getOrElse EmptyDirectoryArtifact(name = pathBelow.head, pathAbove)
            val theNewShit = findOrCreateDirectory(pathAbove :+ pathBelow.head, pathBelow.tail, theRightDirectory)
            SimpleDirectoryArtifact(theRightDirectory.name, theRightDirectory.pathElements,
              replaceOldWithNew(theRightDirectory.artifacts, theNewShit),
              theRightDirectory.uniqueId)
          }
        }

        val newOrModifiedEntry = findOrCreateDirectory(Nil, newArtifact.pathElements, ArtifactSource.this)
        replaceOldWithNew(ArtifactSource.this.artifacts, newOrModifiedEntry)
      }

      override val cachedDeltas: Seq[Delta] = (Option(ArtifactSource.this.cachedDeltas) match {
        case Some(cd) => cd
        case None => Nil
      }) :+ (newArtifact match {
        case d: DirectoryArtifact => DirectoryAdditionDelta(d)
        case f: FileArtifact => FileAdditionDelta(f)
      })
    }

  def filterArtifactSource(df: ArtifactDirFilter, ff: ArtifactFileFilter): ArtifactSource = {
    def filterArtifacts(artifacts: Seq[Artifact]) = artifacts collect {
      case d: DirectoryArtifact if df(d.path) => new FilteringDirectoryArtifact(d)
      case f: FileArtifact if ff(f.path) => f
    }

    class FilteringDirectoryArtifact(dir: DirectoryArtifact)
      extends DirectoryArtifact
        with DirectoryBasedArtifactContainer {

      override val name = dir.name
      override val pathElements = dir.pathElements

      override def artifacts: Seq[Artifact] = filterArtifacts(dir.artifacts)
    }

    new ArtifactSource with DirectoryBasedArtifactContainer {
      override val id = ArtifactSource.this.id

      override val artifacts: Seq[Artifact] = filterArtifacts(ArtifactSource.this.artifacts)

      override val cachedDeltas: Seq[Delta] = {
        val deletedArtifacts: Seq[Artifact] = ArtifactSource.this.artifacts.filter(a => !artifacts.exists(_.path.equals(a.path)))
        ArtifactSource.this.cachedDeltas ++ (deletedArtifacts map {
          case d: DirectoryArtifact => DirectoryDeletionDelta(d)
          case f: FileArtifact => FileDeletionDelta(f)
        })
      }
    }
  }

  /**
    * Filter down to artifacts matching the given predicates.
    */
  def filter(dirFilter: DirFilter, fileFilter: FileFilter): ArtifactSource = {
    def filterArtifacts(artifacts: Seq[Artifact]) = artifacts collect {
      case d: DirectoryArtifact if dirFilter(d) => new FilteringDirectoryArtifact(d)
      case f: FileArtifact if fileFilter(f) => f
    }

    class FilteringDirectoryArtifact(dir: DirectoryArtifact)
      extends DirectoryArtifact
        with DirectoryBasedArtifactContainer {

      override val name = dir.name
      override val pathElements = dir.pathElements

      override def artifacts: Seq[Artifact] = filterArtifacts(dir.artifacts)
    }

    new ArtifactSource with DirectoryBasedArtifactContainer {
      override val id = ArtifactSource.this.id

      override val artifacts: Seq[Artifact] = filterArtifacts(ArtifactSource.this.artifacts)

      override val cachedDeltas: Seq[Delta] = {
        val deletedArtifacts: Seq[Artifact] = ArtifactSource.this.artifacts.filter(a => !artifacts.exists(_.path.equals(a.path)))
        ArtifactSource.this.cachedDeltas ++ (deletedArtifacts map {
          case d: DirectoryArtifact => DirectoryDeletionDelta(d)
          case f: FileArtifact => FileDeletionDelta(f)
        })
      }
    }
  }

  /**
    * Java 8 version of filter.
    */
  def filter(dirFilter: JFunction[DirectoryArtifact, Boolean],
             fileFilter: JFunction[FileArtifact, Boolean]): ArtifactSource =
    filter(d => dirFilter.apply(d), f => fileFilter.apply(f))

  def removeEmptyDirectories(): ArtifactSource = filter(d => d.allFiles.nonEmpty, f => true)

  def +(as: ArtifactSource): ArtifactSource = this plus as

  def plus(additionalArtifacts: Seq[Artifact]): ArtifactSource = {
    // TODO must be able to do this in a nicer, more functional, way
    var as = this
    for (a <- additionalArtifacts) {
      as = as plus a
    }
    as
  }

  def Δ(from: ArtifactSource): Deltas = deltaFrom(from)

  /**
    * Return the changes this ArtifactSource represents against the from ArtifactSource.
    *
    * @param from ArtifactSource (presumably a previous version) to compare this ArtifactSource against
    * @return a set of deltas
    */
  def deltaFrom(from: ArtifactSource): Deltas = {
    val deltas: Seq[Delta] =
      if (this.id.equals(from.id)) {
        // Remove deltas that deal with files that get deleted later
        val deltas = cachedDeltas.filter(d => d match {
          case fad: FileAdditionDelta => findFile(fad.newFile.path).isDefined
          case fud: FileUpdateDelta => findFile(fud.updatedFile.path).isDefined
          case _ => true
        })
        Option(from.cachedDeltas) match {
          case Some(cd) => deltas filterNot (cd contains)
          case _ => deltas
        }
      } else {
        val ourFiles = allFiles
        val fromFiles = from.allFiles
        val additions: Seq[Delta] =
          for {
            f <- ourFiles if from.findFile(f.path).isEmpty
          } yield FileAdditionDelta(f)

        val deletions: Seq[Delta] =
          for {
            f <- fromFiles if this.findFile(f.path).isEmpty
          } yield FileDeletionDelta(f)

        val updates: Seq[Delta] =
          for {
            oldFile <- fromFiles
            currentFile <- this.findFile(oldFile.path)
            if modified(oldFile, currentFile)
          } yield FileUpdateDelta(oldFile = oldFile, updatedFile = currentFile)

        additions ++ deletions ++ updates
      }
    Option(from.collisions) match {
      case Some(c) => Deltas(deltas, collisions filterNot (c contains))
      case _ => Deltas(deltas, collisions)
    }
  }

  private def modified(oldFile: FileArtifact, currentFile: FileArtifact): Boolean =
    oldFile.contentLength != currentFile.contentLength || !oldFile.content.equals(currentFile.content)

  def +(additionalArtifacts: Seq[Artifact]): ArtifactSource =
    this.plus(additionalArtifacts)

  def +(a: Artifact): ArtifactSource = this plus a

  /**
    * Remove the file with the given path, if found.
    *
    * @param path Path of file to remove, if found
    * @return an ArtifactSource
    */
  def delete(path: String): ArtifactSource = filter(d => true, f => !f.path.equals(path))

  /**
    * Remove the file with the given path, if found
    *
    * @param path Path of file to remove, if found
    * @return an ArtifactSource
    */
  def -(path: String): ArtifactSource = this delete path

  /**
    * Return ArtifactSource containing artifacts under this path.
    *
    * @param path the path
    * @return the ArtifactSource under this path
    */
  def underPath(path: String): ArtifactSource = {
    if (path == null || "".equals(path))
      this
    else {
      val dir = findDirectory(path)
      if (dir.isDefined) {
        // TODO this is fragile
        val pathTok = path.split("/")
        ArtifactSource.repathed(getIdString + "/" + path, dir.get.allFiles, a => a.pathElements.drop(pathTok.length))
      } else if (findFile(path).isDefined)
        throw new IllegalArgumentException(s"Cannot drill into directory '$path': It's a file not a directory")
      else
        new EmptyArtifactSource(this.id + "/" + path)
    }
  }

  /**
    * Add path above. e.g. file dir/name becomes xx/yy/dir/name if path above is xx/yy.
    *
    * @param path path string. May include /s
    * @return the ArtifactSource above this path
    */
  def withPathAbove(path: String): ArtifactSource = {
    // TODO share with underPath
    if (path == null || "".equals(path))
      this
    else {
      val pathTok = path.split("/")
      val newPathElements = pathTok.toSeq
      repathed(path + "/" + getIdString, a => newPathElements ++ a.pathElements)
    }
  }

  /**
    * Add path above.
    */
  def /:(path: String): ArtifactSource = withPathAbove(path)

  /**
    * Change paths inside this ArtifactSource.
    */
  def repathed(idSuffix: String, newPathFor: Artifact => Seq[String]): ArtifactSource = {
    ArtifactSource.repathed(this.getIdString + idSuffix, this.allFiles, newPathFor)
  }

  /**
    * Return ArtifactSource containing artifacts under this path.
    */
  def /(path: String): ArtifactSource = underPath(path)

  /**
    * Split this ArtifactSource into two, given criteria.
    *
    * @param crit criteria describing split
    * @return two ArtifactSources
    */
  def split(crit: SplitCriteria): ArtifactSourceSplit = {
    val a = this.filter(d => true, crit.shared || crit.toA)
    val b = this.filter(d => true, crit.shared || !crit.toA)
    (a, b)
  }

  /**
    * Edit files in place.
    *
    * @param fe a file editor
    * @return ArtifactSource with files updated
    */
  def edit(fe: FileEditor): ArtifactSource = {
    val fuds = new ListBuffer[FileUpdateDelta]()
    val filesToUse = allFiles map {
      case oldFile if fe.canAffect(oldFile) =>
        val newFile = fe.edit(oldFile)
        if (newFile.equals(oldFile))
          oldFile
        else {
          fuds.append(FileUpdateDelta(oldFile, newFile))
          newFile
        }
      case oldFile => oldFile
    }

    if (fuds.isEmpty)
      this
    else {
      val deltas = ArtifactSource.this.cachedDeltas ++ fuds
      val collisions = ArtifactSource.this.collisions ++ fuds
      new SimpleFileBasedArtifactSource(this.id, filesToUse, deltas, collisions)
    }
  }

  def ✎(fe: FileEditor) = edit(fe)

  private val cacheIfNecessary: Artifact => Artifact = {
    case d: DirectoryArtifact => SimpleDirectoryArtifact(d.name, d.pathElements,
      d.artifacts.map(a => cacheIfNecessary(a)), d.uniqueId)
    case f: FileArtifact => cacheFileIfNecessary(f)
  }

  private def cacheFileIfNecessary(fa: FileArtifact): FileArtifact = fa match {
    case f: FileArtifact if f.isCached => f
    case f: FileArtifact => new ByteArrayFileArtifact(f)
  }

  /**
    * Return a cached version of this ArtifactSource.
    */
  def cached: ArtifactSource = {
    new ArtifactSource with DirectoryBasedArtifactContainer {
      override val id = ArtifactSource.this.id
      override val artifacts: Seq[Artifact] = ArtifactSource.this.artifacts.map(a => cacheIfNecessary(a))
    }
  }

  override def toString = s"${
    getClass.getSimpleName
  }:${
    artifacts.size
  } artifacts"
}