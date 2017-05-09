package com.atomist.source.file

import java.io._

import com.atomist.source._
import com.atomist.source.file.ClassPathArtifactSource.classPathResourceToFile
import com.atomist.util.BinaryDecider
import org.apache.commons.io.{FileUtils, IOUtils}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}

class ZipFileArtifactSourceReaderTest extends FlatSpec with Matchers {

  import ZipFileArtifactSourceReaderTest._

  "ZipFileArtifactSourceReader" should "read zip file with directory after files" in {
    val zid = leinTemplateZileFileId
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
    val zid = springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    TestUtils.directoryPathElementsShouldExistAndContainName(zipSource)
  }

  it should "read non-empty zip file and verify contents" in {
    val zid = springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    zipSource.allFiles.size should be > 0
    zipSource.allFiles.foreach(f => if (!f.name.equals("application.properties")) f.content.length should be > 0)

    // Look for Java source
    val javaFile = zipSource.findFile("src/main/java/com/example/DemoApplication.java")
    javaFile shouldBe defined
    val javaSource = javaFile.get.content
    javaSource.contains("import") shouldBe true
    javaSource.contains("@SpringBootApplication") shouldBe true
    BinaryDecider.isBinaryContent(javaSource) shouldBe false

    // Look for a jar file
    val jarFile = zipSource.findFile(".mvn/wrapper/maven-wrapper.jar")
    jarFile shouldBe defined
    val content = IOUtils.toByteArray(jarFile.get.inputStream())
    BinaryDecider.isBinaryContent(content) shouldBe true

    val tmpFile = File.createTempFile("tmp", ".jar")
    tmpFile.deleteOnExit()
    FileUtils.copyInputStreamToFile(jarFile.get.inputStream(), tmpFile)
    val jar = ZipFileInput(tmpFile)
    val jarSource = ZipFileArtifactSourceReader.fromZipSource(jar)
    jarSource.allFiles.foreach(f => if (f.name.equals("MANIFEST.MF")) f.content.length shouldEqual 298)
    tmpFile.delete()

    // Look for an executable file and verify executable permission
    val execFile = zipSource.findFile("mvnw")
    execFile shouldBe defined
    execFile.get.mode should be(FileArtifact.ExecutableMode)
  }

  it should "read zip file and verify file permissions" in {
    val zid = springRestServiceZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    zipSource.allFiles.size should be > 0
    zipSource.allFiles.foreach(f => if (!f.name.equals("pom.xml")) f.content.length should be > 0)

    // Look for an non-executable file and verify permissions
    val cmdFile = zipSource.findFile("project/mvnw.cmd")
    cmdFile shouldBe defined
    if (!System.getProperty("os.name").contains("indows"))
      cmdFile.get.mode should be(FileArtifact.DefaultMode)

    // Look for an executable file and verify executable permission
    val execFile = zipSource.findFile("project/mvnw")
    execFile shouldBe defined
    execFile.get.mode should be(FileArtifact.ExecutableMode)
  }

  it should "preserve empty directories" in {
    val zid = springBootZipFileId
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    val resourceDir = zipSource.findDirectory("src/test/resources")
    resourceDir shouldBe defined
  }

  it should "read large zip file" in {
    // val start = System.currentTimeMillis()
    val zid = travisRugsZip
    val zipSource = ZipFileArtifactSourceReader.fromZipSource(zid)
    // println(s"elapsed time = ${System.currentTimeMillis() - start} ms")
    val atomistDir = zipSource.findDirectory(".atomist")
    atomistDir shouldBe defined
  }

  ignore should "handle multiple zip files at the same time" in {
    val f1: Future[ArtifactSource] = Future {
      ZipFileArtifactSourceReader.fromZipSource(travisRugsZip)
    }

    val f2: Future[ArtifactSource] = Future {
      ZipFileArtifactSourceReader.fromZipSource(springBootZipFileId)
    }

    val f3: Future[ArtifactSource] = Future {
      ZipFileArtifactSourceReader.fromZipSource(springRestServiceZipFileId)
    }

    Await.result(Future.sequence(Seq(f1, f2, f3)), Duration(60, SECONDS))
      .map(_.id.name).foreach(println(_))

    Thread sleep 2000
  }
}

object ZipFileArtifactSourceReaderTest {

  def springBootZipFileId = ZipFileInput(classPathResourceToFile("springboot1.zip"))

  def springRestServiceZipFileId = ZipFileInput(classPathResourceToFile("spring-rest-service.zip"))

  def leinTemplateZileFileId = ZipFileInput(classPathResourceToFile("simple-lein-project-1.0.0.zip"))

  def travisRugsZip = ZipFileInput(classPathResourceToFile("travis-rugs.zip"))
}