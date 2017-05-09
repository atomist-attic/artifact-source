package com.atomist.source.file

import java.io.{File, IOException, InputStream}
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.Executors
import java.util.{Set => JSet}

import com.atomist.source._
import com.atomist.util.FilePermissions.fromMode
import com.atomist.util.Utils.withCloseable
import org.apache.commons.compress.archivers.zip.{AsiExtraField, ZipArchiveEntry, ZipFile}
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ZipFileInput(is: InputStream) extends ArtifactSourceIdentifier {

  def this(file: File) = this(FileUtils.openInputStream(file))

  override val name: String = s"$is"
}

object ZipFileInput {

  def apply(file: File) = new ZipFileInput(file)
}

/**
  * Reads an ArtifactSource from a zip file.
  *
  * Must use the commons-compress ZipFile instead of ZipArchiveInputStream - see
  * https://commons.apache.org/proper/commons-compress/zip.html
  */
object ZipFileArtifactSourceReader {

  implicit lazy val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))

  private val IsWindows = System.getProperty("os.name").contains("indows")

  def fromZipSource(id: ZipFileInput): ArtifactSource = {
    val zipFile = getZipFile(id)
    val targetFolder = createTargetFolder
    withCloseable(zipFile)(zf =>
      Await.ready(Future.sequence(processZipEntries(zf, targetFolder)), Duration(60, SECONDS)).onComplete {
        case Success(_) =>
        case Failure(e) => throw ArtifactSourceCreationException(s"Failed to create artifact source", e)
      })

    val fid = NamedFileSystemArtifactSourceIdentifier(substringAfterLast(targetFolder.getName, "/"), targetFolder)
    FileSystemArtifactSource(fid)
  }

  private def getZipFile(id: ZipFileInput) =
    try {
      val tmpFile = File.createTempFile("tmp", ".zip")
      tmpFile.deleteOnExit()
      FileUtils.copyInputStreamToFile(id.is, tmpFile)
      new ZipFile(tmpFile)
    } catch {
      case e: IOException => throw ArtifactSourceCreationException("Failed to copy zip contents to temp file", e)
    }

  private def createTargetFolder =
    try {
      val targetFolder = Files.createTempDirectory("artifact-source-").toFile
      targetFolder.deleteOnExit()
      targetFolder
    } catch {
      case e: IOException => throw ArtifactSourceCreationException("Failed to create folder for artifact source", e)
    }

  private def processZipEntries(zipFile: ZipFile, targetFolder: File) =
    for (entry <- zipFile.getEntries.asScala.toList) yield Future {
      writeZipEntryToFile(entry, zipEntryInputStream(zipFile), targetFolder)
    }

  private def zipEntryInputStream(zipFile: ZipFile)(entry: ZipArchiveEntry) = zipFile.getInputStream(entry)

  private def writeZipEntryToFile(entry: ZipArchiveEntry,
                                  is: (ZipArchiveEntry) => InputStream,
                                  targetFolder: File): Unit = {
    val pathName = entry.getName
    val path = if (IsWindows) pathName.replace(":", "_") else pathName
    val file = Paths.get(path).toFile
    val newPath = Paths.get(targetFolder.getPath, file.getPath)

    if (file.isDirectory || entry.isDirectory || entry.isUnixSymlink)
      Files.createDirectories(newPath)
    else {
      val parentDir = newPath.toFile.getParentFile.toPath
      if (!Files.exists(parentDir))
        parentDir.toFile.mkdirs

      val perms = getPermissions(entry)
      val newFile = createFile(newPath, perms)
      FileUtils.copyInputStreamToFile(is(entry), newFile)
    }
  }

  private def getPermissions(entry: ZipArchiveEntry): JSet[PosixFilePermission] = {
    val unixMode = entry.getUnixMode
    val mode = unixMode match {
      case 0 =>
        entry.getExtraFields().collectFirst {
          case aef: AsiExtraField => aef.getMode
          case _ => FileArtifact.DefaultMode
        }.getOrElse(FileArtifact.DefaultMode)
      case _ => unixMode
    }
    fromMode(mode)
  }

  private def createFile(newPath: Path, perms: JSet[PosixFilePermission]) =
    Try {
      val fileAttributes = PosixFilePermissions.asFileAttribute(perms)
      Files.createFile(newPath, fileAttributes).toFile
    } match {
      case Success(f) => f
      case Failure(_: UnsupportedOperationException) =>
        // In case of Windows
        val f = Files.createFile(newPath).toFile
        f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
        f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
        f
      case Failure(t: Throwable) =>
        throw t
    }

  private def substringAfterLast(s: String, k: String) =
    s.lastIndexOf(k) match {
      case -1 => s;
      case i => s.substring(i + s.length)
    }
}