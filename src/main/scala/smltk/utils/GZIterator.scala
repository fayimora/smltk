package smltk.utils

import java.io._
import java.util.zip.GZIPInputStream

case class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
  override def hasNext() = reader.ready
  override def next() = if(!hasNext()) {close(); ""} else reader.readLine()
  def close() = reader.close()
}

object GZIterator {
  def apply(filePath: String) = {
    new BufferedReaderIterator(
      new BufferedReader(
        new InputStreamReader(
          new GZIPInputStream(
            new FileInputStream(filePath)), "UTF-8")))
  }
}
