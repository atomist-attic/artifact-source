package com.atomist.source.file

import java.io.{File, IOException, InputStream}
import java.nio.file.attribute.{FileAttribute, PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path, Paths}
import java.util.{Set => JSet}

import com.atomist.source._
import com.atomist.util.FilePermissions.fromMode
import org.apache.commons.compress.archivers.zip.{AsiExtraField, ZipArchiveEntry, ZipFile}
import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class ZipFileInput(is: InputStream) extends ArtifactSourceIdentifier {

  override val name: String = s"$is"
}

/**
  * Reads an ArtifactSource from a zip file.
  *
  * Must use the commons-compress ZipFile instead of ZipArchiveInputStream - see
  * https://commons.apache.org/proper/commons-compress/zip.html
  */
object ZipFileArtifactSourceReader {

  private val IsWindows = System.getProperty("os.name").contains("indows")

  def fromZipSource(id: ZipFileInput): ArtifactSource = {
    val tmpFile = File.createTempFile("tmp", ".zip")
    tmpFile.deleteOnExit()
    try {
      FileUtils.copyInputStreamToFile(id.is, tmpFile)
    } catch {
      case e: IOException => throw ArtifactSourceAccessException(s"Failed to copy zip contents to temp file: ${e.getMessage}", e)
    }

    val rootFile = Files.createTempDirectory("tmp").toFile
    rootFile.deleteOnExit()

    val zipFile = try {
      new ZipFile(tmpFile)
    } catch {
      case e: IOException => throw ArtifactSourceAccessException(s"Expected zip entries in $id but none was found", e)
    }

    unzipFile(zipFile.getEntries.asScala.toList, zipEntryInputStream(zipFile) _, rootFile)
    ZipFile.closeQuietly(zipFile)
    FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(rootFile))
  }

  private def zipEntryInputStream(zipFile: ZipFile)(entry: ZipArchiveEntry) = zipFile.getInputStream(entry)

  private def unzipFile(entries: List[ZipArchiveEntry],
                        is: (ZipArchiveEntry) => InputStream,
                        targetFolder: File): Unit = {
    @tailrec
    def unzipEntries(entryList: List[ZipArchiveEntry],
                     is: (ZipArchiveEntry) => InputStream,
                     targetFolder: File): Unit = {
      entryList match {
        case entry :: entries =>
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
            val fileAttributes = PosixFilePermissions.asFileAttribute(perms)
            val newFile = createFile(newPath, perms, fileAttributes)
            FileUtils.copyInputStreamToFile(is(entry), newFile)
          }

          unzipEntries(entries, is, targetFolder)
        case _ =>
      }
    }

    def createFile(newPath: Path,
                   perms: JSet[PosixFilePermission],
                   fileAttributes: FileAttribute[JSet[PosixFilePermission]]) = {
      Try(Files.createFile(newPath, fileAttributes).toFile) match {
        case Success(f) => f
        case Failure(_: UnsupportedOperationException) =>
          // In case of windows
          val f = Files.createFile(newPath).toFile
          f.setExecutable(perms contains PosixFilePermission.OWNER_EXECUTE)
          f.setWritable(perms contains PosixFilePermission.OWNER_WRITE)
          f
        case Failure(t: Throwable) =>
          throw ArtifactSourceCreationException(s"Failed to create file '${newPath.toString}'", t)
      }
    }

    def getPermissions(entry: ZipArchiveEntry): JSet[PosixFilePermission] = {
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

    unzipEntries(entries, is, targetFolder)
  }
}