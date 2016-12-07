package com.atomist.source.file

import java.io.File
import java.nio.file.{Files, Path}

import com.atomist.source.{ArtifactSourceCreationException, SimpleSourceUpdateInfo, TestUtils}
import org.scalatest.{FlatSpec, Matchers}

class FileSystemArtifactSourceWriterTest extends FlatSpec with Matchers {

  val helloSource = ClassPathArtifactSource.toArtifactSource("java-source/HelloWorldService.java")

  val validSource = ClassPathArtifactSource.toArtifactSource("spring-boot")

  private def newTemporaryDir: Path = Files.createTempDirectory(getClass.getSimpleName)

  val fWriter = new FileSystemArtifactSourceWriter
//
//  "FileSystemArtifactSourceWriter" should "fail with bogus file id" in {
//    validSource.empty shouldBe false
//    val fid = FileSystemArtifactSourceIdentifier(new File(""))
//    an[ArtifactSourceCreationException] should be thrownBy fWriter.write(validSource, fid, SimpleSourceUpdateInfo(getClass.getName))
//  }
//
//  it should "not allow writing to same id" in {
//    val fid = helloSource.id.asInstanceOf[FileSystemArtifactSourceIdentifier]
//    an[ArtifactSourceCreationException] should be thrownBy fWriter.write(validSource, fid, SimpleSourceUpdateInfo(getClass.getName))
//  }
//
//  it should "produce result with equivalent contents" in {
//    validSource.empty shouldBe false
//    val fid = FileSystemArtifactSourceIdentifier(newTemporaryDir.toFile)
//    fWriter.write(validSource, fid, SimpleSourceUpdateInfo(getClass.getName))
//    val as = new FileSystemArtifactSource(fid)
//    as.artifacts.nonEmpty shouldBe true
//    as.artifacts.size shouldEqual validSource.artifacts.size
//    TestUtils.verify(validSource, as)
//  }

  it should "produce files with equivalent contents when read in again" in {
    validSource.empty shouldBe false
    val fid = FileSystemArtifactSourceIdentifier(newTemporaryDir.toFile)
    fWriter.write(validSource, fid, SimpleSourceUpdateInfo(getClass.getName))
    val as = new FileSystemArtifactSource(fid)
    as.artifacts.size shouldEqual validSource.artifacts.size
    TestUtils.verify(validSource, as)
  }
}
