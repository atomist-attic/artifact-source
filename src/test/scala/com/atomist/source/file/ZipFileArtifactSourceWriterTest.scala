package com.atomist.source.file

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

import com.atomist.source.{ArtifactSource, SimpleSourceUpdateInfo, TestUtils}
import org.scalatest.{FlatSpec, Matchers}

class ZipFileArtifactSourceWriterTest extends FlatSpec with Matchers {

  val zid = ZipFileArtifactSourceReaderTest.SpringBootZipFileId
  val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
  val zw = ZipFileArtifactSourceWriter

  "ZipFileArtifactSourceWriter" should "create accurate copy of files" in {
    val baos = new ByteArrayOutputStream
    val zo = StreamingZipFileOutput("foobar", baos)
    zw.write(zipSource, zo, SimpleSourceUpdateInfo(getClass.getName)) shouldBe true
    verify(zipSource, new ByteArrayInputStream(baos.toByteArray))
  }

  it should "create readable archive" in {
    val readable = ZipFileArtifactSourceWriter.getReadable(zipSource, "somename")
    val read = ZipFileArtifactSourceReader.fromZipSource(ZipFileInput(readable.is))
    TestUtils.verify(read, zipSource)
  }

  private def verify(from: ArtifactSource, is: InputStream): Unit = {
    val zid = ZipFileInput(is)
    val zs = ZipFileArtifactSourceReader.fromZipSource(zid)
    zs.allDirectories
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
