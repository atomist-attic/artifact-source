package com.atomist.source.file

import java.io.File
import java.nio.file.{Files, Path}

import com.atomist.source.{ArtifactSourceCreationException, SimpleSourceUpdateInfo, TestUtils}
import org.apache.commons.io.FileUtils
import org.scalatest.{FlatSpec, Matchers}

class FileSystemArtifactSourceWriterTest extends FlatSpec with Matchers {

  import FileSystemArtifactSourceWriterTest._

  private def newTemporaryDir: Path = Files.createTempDirectory(getClass.getSimpleName)

  val fWriter = new FileSystemArtifactSourceWriter

  "FileSystemArtifactSourceWriter" should "fail with bogus file id" in {
    ValidSource.empty shouldBe false
    val fid = FileSystemArtifactSourceIdentifier(new File(""))
    an[ArtifactSourceCreationException] should be thrownBy fWriter.write(ValidSource, fid, SimpleSourceUpdateInfo(getClass.getName))
  }

  it should "not allow writing to same id" in {
    val fid = HelloSource.id.asInstanceOf[FileSystemArtifactSourceIdentifier]
    an[ArtifactSourceCreationException] should be thrownBy fWriter.write(ValidSource, fid, SimpleSourceUpdateInfo(getClass.getName))
  }

  it should "produce result with equivalent contents" in {
    ValidSource.empty shouldBe false
    val fid = FileSystemArtifactSourceIdentifier(newTemporaryDir.toFile)
    fWriter.write(ValidSource, fid, SimpleSourceUpdateInfo(getClass.getName))
    val as = FileSystemArtifactSource(fid)
    as.artifacts.nonEmpty shouldBe true
    as.artifacts.size shouldEqual ValidSource.artifacts.size
    TestUtils.verify(ValidSource, as)
    FileUtils.deleteQuietly(newTemporaryDir.toFile)
  }

  it should "produce files with equivalent contents when read in again" in {
    ValidSource.empty shouldBe false
    val fid = FileSystemArtifactSourceIdentifier(newTemporaryDir.toFile)
    fWriter.write(ValidSource, fid, SimpleSourceUpdateInfo(getClass.getName))
    val as = FileSystemArtifactSource(fid)
    as.artifacts.size shouldEqual ValidSource.artifacts.size
    TestUtils.verify(ValidSource, as)
    FileUtils.deleteQuietly(newTemporaryDir.toFile)
  }
}

object FileSystemArtifactSourceWriterTest {

  private val HelloSource = ClassPathArtifactSource.toArtifactSource("java-source/HelloWorldService.java")

  private val ValidSource = ClassPathArtifactSource.toArtifactSource("spring-boot")
}
