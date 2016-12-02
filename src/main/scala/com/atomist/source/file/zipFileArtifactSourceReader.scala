package com.atomist.source.file

import java.io.{File, IOException, InputStream, PushbackInputStream}
import java.nio.charset.Charset
import java.nio.file.Paths

import com.atomist.source.{FileArtifact, _}
import com.atomist.util.BinaryDecider
import com.atomist.util.PathUtils._
import com.atomist.util.Utils.withCloseable
import org.apache.commons.compress.archivers.zip.{AsiExtraField, ZipFile}
import org.apache.commons.io.{FileUtils, IOUtils}

import scala.collection.JavaConverters._

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

  def fromZipSource(id: ZipFileInput): ArtifactSource = {
    val tmpFile = File.createTempFile("tmp", ".zip")
    tmpFile.deleteOnExit()
    try {
      FileUtils.copyInputStreamToFile(id.is, tmpFile)
    } catch {
      case e: IOException => throw ArtifactSourceAccessException(s"Failed to copy zip contents to temp file: ${e.getMessage}")
    }

    val zipFile = try {
      new ZipFile(tmpFile)
    } catch {
      case e: IOException => throw ArtifactSourceAccessException(s"Expected zip entries in $id but none was found")
    }

    try {
      val artifactsRead: Seq[Artifact] =
        zipFile.getEntries().asScala
          .map(entry => {
            val pathName = convertPath(entry.getName)
            val file = Paths.get(pathName).toFile
            if (file.isDirectory || entry.isDirectory || entry.isUnixSymlink) {
              val split = splitPath(pathName)
              val name = split.last
              val pathElements = split.seq.dropRight(1)
              EmptyDirectoryArtifact(name, pathElements)
            } else {
              val unixMode = entry.getUnixMode
              val mode = unixMode match {
                case 0 =>
                  entry.getExtraFields().collectFirst {
                    case aef: AsiExtraField => aef.getMode
                    case _ => FileArtifact.DefaultMode
                  }.getOrElse(FileArtifact.DefaultMode)
                case _ => unixMode
              }
              withCloseable(zipFile.getInputStream(entry))(is => {
                val pis = new PushbackInputStream(is, 10)
                val peekBytes = new Array[Byte](10)
                pis.read(peekBytes)
                val isBinary = BinaryDecider.isBinaryContent(peekBytes)
                pis.unread(peekBytes)
                if (isBinary) {
                  val bytes = IOUtils.toByteArray(pis)
                  ByteArrayFileArtifact(pathName, bytes, mode)
                } else {
                  val content = IOUtils.toString(pis, Charset.defaultCharset())
                  StringFileArtifact(pathName, content, mode, None)
                }
              })
            }
          }).toSeq
      val (emptyDirectories, other) = artifactsRead.partition(_.isInstanceOf[EmptyDirectoryArtifact])
      val emptyDirectoriesFirstArtifactsRead: Seq[Artifact] = emptyDirectories ++ other
      EmptyArtifactSource(id) + emptyDirectoriesFirstArtifactsRead
    } finally {
      zipFile.close()
      tmpFile.delete()
    }
  }
}
