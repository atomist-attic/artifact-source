package com.atomist.util

object BinaryDecider {

  def isBinaryContent(content: Array[Byte]): Boolean = isBinary(content, CharTypeCounts(0, 0))

  def isBinaryContent(content: String): Boolean = isBinaryContent(content.getBytes())

  private def isBinary(data: Array[Byte], counts: CharTypeCounts): Boolean = data.headOption match {
    case None => counts.isBinary
    case Some(b) if b < 0x09 => true
    case Some(b) if Seq(0x09, 0x0A, 0x0C, 0x0D).contains(b) || (b >= 0x20 && b <= 0x7E) => isBinary(data.tail, counts.incAscii)
    case _ => isBinary(data.tail, counts.incBinary)
  }

  private case class CharTypeCounts(ascii: Int, binary: Int) {

    def isBinary = if (binary == 0) false else 100 * binary / (ascii + binary) > 95

    def incAscii = copy(ascii = ascii + 1)

    def incBinary = copy(binary = binary + 1)
  }
}
