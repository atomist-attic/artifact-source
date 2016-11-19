package com.atomist.source.file

import java.io.{BufferedInputStream, File, IOException}
import java.nio.charset.Charset

import com.atomist.source._
import org.apache.commons.io.FileUtils

class FileSystemArtifactSourceWriter {

  def write(as: ArtifactSource, id: FileSystemArtifactSourceIdentifier, sui: SourceUpdateInfo): File = {
    if (id == as.id)
      throw ArtifactSourceCreationException(s"Cannot write to same $id")
    // Try to create the directory if it doesn't exist
    if (!id.rootFile.canWrite)
      id.rootFile.mkdir()
    if (!id.rootFile.canWrite)
      throw ArtifactSourceCreationException(s"Cannot write to file $id")
    try {
      for (f <- as.allFiles)
        write(f, id.rootFile)
      // We won't have gotten empty directories yet
      for (d <- as.allDirectories if d.empty)
        createEmptyDir(d, id.rootFile)
      id.rootFile
    } catch {
      case e: IOException =>
        throw ArtifactSourceCreationException(s"Couldn't create artifact with id $id", e)
    }
  }

  def write(fa: FileArtifact, rootFile: File) {
    val newFile = new File(rootFile, fa.path)

    fa match {
      case ns: NonStreamedFileArtifact =>
        FileUtils.writeStringToFile(newFile, fa.content, Charset.defaultCharset())
      case sa: StreamedFileArtifact =>
        FileUtils.copyInputStreamToFile(new BufferedInputStream(sa.inputStream()), newFile)
      case ba: ByteArrayFileArtifact =>
        FileUtils.writeByteArrayToFile(newFile, ba.bytes)
    }

    if (fa.mode == FileArtifact.ExecutableMode && !newFile.setExecutable(true, false)) {
      throw ArtifactSourceCreationException(s"Couldn't set executable file permissions for file $newFile")
    }
  }

  private def createEmptyDir(da: DirectoryArtifact, rootFile: File) {
    val dir = new File(rootFile, da.path)
    if (!dir.exists())
      dir.mkdirs()
  }
}
