package com.atomist.source.file

import java.io._

import com.atomist.source._
import com.atomist.source.file.ClassPathArtifactSource.classPathResourceToFile
import org.apache.commons.io.FileUtils
import org.scalatest.{FlatSpec, Matchers}

object ZipFileArtifactSourceReaderTest {

  def springBootZipFileId = {
    val f = classPathResourceToFile("springboot1.zip")
    ZipFileInput(new FileInputStream(f))
  }

  def springRestServiceZipFileId = {
    val f = classPathResourceToFile("spring-rest-service.zip")
    ZipFileInput(new FileInputStream(f))
  }

  def leinTemplateZileFileId = {
    val f = classPathResourceToFile("simple-lein-project-1.0.0.zip")
    ZipFileInput(new FileInputStream(f))
  }
}

class ZipFileArtifactSourceReaderTest extends FlatSpec with Matchers {

  it should "read zip file with directory after files" in {
    val zid = ZipFileArtifactSourceReaderTest.leinTemplateZileFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)

    val cljFile = zipSource.findFile("project.clj")
    cljFile shouldBe defined

    val rugFile = zipSource.findFile(".atomist/editors/SimpleLeinClojureProject.rug")
    rugFile shouldBe defined
  }

  it should "reject bogus zip file content" in {
    val is = new ByteArrayInputStream("Definitely NOT a zip file".getBytes())
    val zid = ZipFileInput(is)
    an[ArtifactSourceException] should be thrownBy ZipFileArtifactSourceReader.fromZipSource(zid)
  }

  it should "read non-empty zip file and verify directory structure" in {
    val zid = ZipFileArtifactSourceReaderTest.springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    TestUtils.directoryPathElementsShouldExistAndContainName(zipSource)
  }

  it should "read non-empty zip file and verify contents" in {
    val zid = ZipFileArtifactSourceReaderTest.springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    zipSource.allFiles.size should be > 0
    zipSource.allFiles.foreach(f => if (!f.name.equals("application.properties")) f.content.length should be > 0)

    // Look for Java source
    val javaFile = zipSource.findFile("src/main/java/com/example/DemoApplication.java")
    javaFile shouldBe defined
    javaFile.get shouldBe a[StringFileArtifact]
    val javaSource = javaFile.get.content
    javaSource.contains("import") shouldBe true
    javaSource.contains("@SpringBootApplication") shouldBe true

    // Look for a jar file
    val jarFile = zipSource.findFile(".mvn/wrapper/maven-wrapper.jar")
    jarFile shouldBe defined
    jarFile.get shouldBe a[ByteArrayFileArtifact]

    val tmpFile = File.createTempFile("tmp", ".jar")
    tmpFile.deleteOnExit()
    FileUtils.copyInputStreamToFile(jarFile.get.inputStream(), tmpFile)
    val jar = ZipFileInput(new FileInputStream(tmpFile))
    val jarSource = ZipFileArtifactSourceReader.fromZipSource(jar)
    jarSource.allFiles.foreach(f => if (f.name.equals("MANIFEST.MF")) f.content.length shouldEqual 298)
    tmpFile.delete()

    // Look for an executable file and verify executable permission
    val execFile = zipSource.findFile("mvnw")
    execFile shouldBe defined
    execFile.get.mode should be(FileArtifact.ExecutableMode)
  }

  it should "read zip file and verify file permissions" in {
    val zid = ZipFileArtifactSourceReaderTest.springRestServiceZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    zipSource.allFiles.size should be > 0
    zipSource.allFiles.foreach(f => if (!f.name.equals("pom.xml")) f.content.length should be > 0)

    // Look for an non-executable file and verify permissions
    val cmdFile = zipSource.findFile("project/mvnw.cmd")
    cmdFile shouldBe defined
    cmdFile.get shouldBe a[StringFileArtifact]
    cmdFile.get.mode should be(FileArtifact.DefaultMode)

    // Look for an executable file and verify executable permission
    val execFile = zipSource.findFile("project/mvnw")
    execFile shouldBe defined
    execFile.get shouldBe a[StringFileArtifact]
    execFile.get.mode should be(FileArtifact.ExecutableMode)
  }

  it should "preserve empty directories" in {
    val zid = ZipFileArtifactSourceReaderTest.springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    val resourceDir = zipSource.findDirectory("src/test/resources")
    resourceDir shouldBe defined
  }

  private def validateTargetDirectory(s: ArtifactSource): Unit = {
    val files = s.allFiles
    files.exists(_.name contains ".vm")
  }
}
