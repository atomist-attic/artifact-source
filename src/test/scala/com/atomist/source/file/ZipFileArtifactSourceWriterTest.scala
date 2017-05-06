package com.atomist.source.file

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

import com.atomist.source.{ArtifactSource, SimpleSourceUpdateInfo, TestUtils}
import org.scalatest.{FlatSpec, Matchers}

class ZipFileArtifactSourceWriterTest extends FlatSpec with Matchers {

  import ZipFileArtifactSourceWriterTest._

  "ZipFileArtifactSourceWriter" should "create accurate copy of files" in {
    val baos = new ByteArrayOutputStream
    val zo = StreamingZipFileOutput("foobar", baos)
    Writer.write(ZipSource, zo, SimpleSourceUpdateInfo(getClass.getName)) shouldBe true
    verify(ZipSource, new ByteArrayInputStream(baos.toByteArray))
  }

  it should "create readable archive" in {
    val readable = ZipFileArtifactSourceWriter.getReadable(ZipSource, "somename")
    val read = ZipFileArtifactSourceReader.fromZipSource(ZipFileInput(readable.is))
    TestUtils.verify(read, ZipSource)
  }

  private def verify(from: ArtifactSource, is: InputStream): Unit = {
    val zid = ZipFileInput(is)
    val zs = ZipFileArtifactSourceReader.fromZipSource(zid)
    zs.allDirectories
    zs.allFiles
    zs.allFiles.size should equal(from.allFiles.size)
    for (f <- from.allFiles) {
      val found = zs.findFileByPath(f.name, f.pathElements)
      withClue(s"Looking for $f") {
        found shouldBe defined
      }
      withClue(s"Looking at content length for $f") {
        found.get.contentLength shouldBe f.contentLength
      }
    }

    for (d <- from.allDirectories) {
      val found = zs.findDirectoryByPath(d.pathElements)
      withClue(s"Looking for $d") {
        found shouldBe defined
      }
    }
  }
}

object ZipFileArtifactSourceWriterTest {

  private val Zid = ZipFileArtifactSourceReaderTest.springBootZipFileId
  private val ZipSource = ZipFileArtifactSourceReader.fromZipSource(Zid)
  private val Writer = ZipFileArtifactSourceWriter
}

