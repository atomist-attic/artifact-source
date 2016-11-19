package com.atomist.source.file

import java.io.{ByteArrayInputStream, InputStream, OutputStream}

import com.atomist.source._
import com.atomist.util.Utils._
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.zip.{AsiExtraField, ZipArchiveEntry}
import org.apache.commons.io.IOUtils
import org.apache.commons.io.output.ByteArrayOutputStream

/**
  * Streaming output for zip file.
  *
  * @param name name of the ArtifactSource this represents
  * @param os output stream to use to write content
  */
case class StreamingZipFileOutput(name: String, os: OutputStream) extends ArtifactSourceIdentifier

case class ReadableZipFileOutput(name: String, is: InputStream) extends ArtifactSourceIdentifier

object ZipFileArtifactSourceWriter {

  def write(as: ArtifactSource, zid: StreamingZipFileOutput, sui: SourceUpdateInfo) = {
    val zip = new ArchiveStreamFactory().createArchiveOutputStream(ArchiveStreamFactory.ZIP, zid.os)
    try {
      for (fa <- as.allFiles) {
        val entry = new ZipArchiveEntry(fa.path)
        val aef = new AsiExtraField
        aef setMode fa.mode
        entry addExtraField aef
        entry setUnixMode fa.mode
        zip putArchiveEntry entry
        withCloseable(fa.inputStream())(is => IOUtils.copy(is, zip))
        zip.closeArchiveEntry()
      }

      // Copy empty directories
      as.allDirectories collect {
        case eda: EmptyDirectoryArtifact =>
          val entry = new ZipArchiveEntry(eda.path)
          zip putArchiveEntry entry
          zip.closeArchiveEntry()
      }
    } finally {
      zip.finish()
      zip.flush()
      zip.close()
      zid.os.flush()
    }
    true
  }

  /**
    * Return an InputSteam for this file.
    */
  // TODO we are keeping this in memory. May not be very efficient.
  def getReadable(as: ArtifactSource, name: String): ReadableZipFileOutput = {
    val baos = new ByteArrayOutputStream()
    val szfo = StreamingZipFileOutput(name, baos)
    write(as, szfo, SimpleSourceUpdateInfo("getReadable"))
    ReadableZipFileOutput(name, new ByteArrayInputStream(baos.toByteArray))
  }
}
