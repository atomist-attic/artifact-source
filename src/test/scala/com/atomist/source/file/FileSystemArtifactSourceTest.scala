package com.atomist.source.file

import java.io.File

import com.atomist.source._
import com.atomist.util.BinaryDecider.isBinaryContent
import org.scalatest._

object FileSystemArtifactSourceTest {

  val AtomistTemplatesSource = ClassPathArtifactSource.toArtifactSource("spring-boot")
}

class FileSystemArtifactSourceTest extends FlatSpec with Matchers {

  import FileSystemArtifactSourceTest._
  import TestUtils._

  it should "handle classpath directory not found" in {
    an[ArtifactSourceException] should be thrownBy (ClassPathArtifactSource toArtifactSource "this is complete nonsense")
  }

  it should "find single file and verify contents" in {
    val classpathSource = ClassPathArtifactSource.toArtifactSource("java-source/HelloWorldService.java")
    val artifacts = classpathSource.artifacts
    val files = artifacts.filter(a => a.isInstanceOf[FileArtifact])
    artifacts.size should be > 0
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0
    aFile.content should have size aFile.contentLength
    isBinaryContent(aFile.content) shouldBe false
  }

  it should "find single image file" in {
    val classpathSource = ClassPathArtifactSource.toArtifactSource("spring-boot/web-template/src/main/resources/atomist-logo-horiz.png")
    val artifacts = classpathSource.artifacts
    val files = artifacts.filter(a => a.isInstanceOf[FileArtifact])
    artifacts.size should be > 0
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0
    isBinaryContent(aFile.content) shouldBe true
  }

  it should "find single binary file" in {
    val classpathSource = ClassPathArtifactSource.toArtifactSource("binary.dat")
    val artifacts = classpathSource.artifacts
    val files = artifacts.filter(a => a.isInstanceOf[FileArtifact])
    artifacts.size should be > 0
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0
    isBinaryContent(aFile.content) shouldBe true
  }

  it should "find single binary executable file" in {
    val classpathSource = ClassPathArtifactSource.toArtifactSource("binary-executable.dat")
    val artifacts = classpathSource.artifacts
    val files = artifacts.filter(a => a.isInstanceOf[FileArtifact])
    artifacts.size should be > 0
    val aFile = files.head.asInstanceOf[FileArtifact]
    aFile.contentLength should be > 0
    isBinaryContent(aFile.content) shouldBe true
    aFile.mode should be(FileArtifact.ExecutableMode)
  }

  it should "find directory" in {
    val artifacts = AtomistTemplatesSource.artifacts
    artifacts.exists(f => f.name contains ".vm")
  }

  it should "find all files via flatten" in {
    validateTargetDirectory(AtomistTemplatesSource)
  }

  // TODO some of these tests are more generic ArtifactSource tests
  it should "be able to cache" in {
    val classpathSource = AtomistTemplatesSource
    classpathSource.allFiles.exists(f => f.isCached) shouldBe false
    validateTargetDirectory(classpathSource)
    val cachedCopy = classpathSource.cached
    cachedCopy.allFiles.exists(f => !f.isCached) shouldBe false
    validateTargetDirectory(cachedCopy)
  }

  it should "be able to filter files" in {
    val s = AtomistTemplatesSource / "atomistTemplates"
    val files = s.allFiles
    files.exists(f => f.name contains ".vm") shouldBe true
    val filtered = s.filter(d => true, f => !f.name.contains(".vm"))
    filtered.allFiles.exists(f => f.name contains ".vm") shouldBe false
    withClue("should leave nothing after filter") {
      filtered.allFiles.isEmpty shouldBe true
    }
  }

  it should "be able to filter directories" in {
    val s = AtomistTemplatesSource
    s.allFiles.exists(f => f.name contains "Application") shouldBe true
    val filtered = s.filter(d => !d.name.contains("spring"), f => true)
    filtered.allFiles.exists(f => f.name contains "Java") shouldBe false
  }

  it should "be able to find existing directory" in {
    val s = AtomistTemplatesSource
    s.directories.nonEmpty shouldBe true
    s.findDirectory("atomistTemplates").isDefined shouldBe true
  }

  it should "not be able to find bogus directory" in {
    val s = AtomistTemplatesSource
    s.directories.nonEmpty shouldBe true
    s.findDirectory("xsdfsdfsdfsdf").isDefined shouldBe false
  }

  it should "reject bogus file rootPath" in {
    val f: File = new File("/this/is/not/a/real.rootPath")
    val fsid = FileSystemArtifactSourceIdentifier(f)
    an[ArtifactSourceException] should be thrownBy new FileSystemArtifactSource(fsid)
  }

  it should "ignore files specified in .atomistignore in test source" in {
    val f = new File(s"$TestIgnoreFilesRoot/dot-atomistignore")
    val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f))
    as.artifacts.size should be(3)
    as.allFiles.size should be(3)
    as.findFile(".atomistignore") shouldBe defined
    as.findFile(".atomist/manifest.yml") shouldBe defined
    as.findFile("thing5") shouldBe defined
  }

  it should "ignore files specified in .atomistignore with .gitignore in test source" in {
    val f = new File(s"$TestIgnoreFilesRoot/dot-atomistignore-and-gitignore")
    val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f))
    as.artifacts.size should be(4)
    as.allFiles.size should be(4)
    as.findFile(".atomistignore") shouldBe defined
    as.findFile(".gitignore") shouldBe defined
    as.findFile(".atomist/manifest.yml") shouldBe defined
    as.findFile("thing5") shouldBe defined
  }

  it should "ignore files specified in .gitignore in 1st test source" in {
    val f = new File(s"$TestIgnoreFilesRoot/dot-gitignore-1")
    val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f))
    as.artifacts.size should be(3)
    as.allFiles.size should be(3)
    as.findFile(".gitignore") shouldBe defined
    as.findFile(".atomist/manifest.yml") shouldBe defined
    as.findFile("thing5") shouldBe defined
  }

  it should "ignore files specified in .gitignore in 2nd test source" in {
    val f = new File(s"$TestIgnoreFilesRoot/dot-gitignore-2")
    val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f))
    as.artifacts.size should be(3)
    as.allFiles.size should be(3)
    as.findFile(".gitignore") shouldBe defined
    as.findFile(".atomist/manifest.yml") shouldBe defined
    as.findFile("thing5") shouldBe defined
  }

  it should "find all files when there is no .gitignore file" in {
    val f = new File(s"$TestIgnoreFilesRoot/no-dot-gitignore")
    val as = new FileSystemArtifactSource(FileSystemArtifactSourceIdentifier(f))
    as.artifacts.size should be(5)
    as.allFiles.size should be(5)
  }

  private def validateTargetDirectory(s: ArtifactSource): Unit = {
    val files = s.allFiles
    files.exists(f => f.name contains ".vm")
  }
}
