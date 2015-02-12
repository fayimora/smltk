package smltk.utils


import java.io.{BufferedReader, InputStreamReader, File, FileInputStream}
import java.util.zip.GZIPInputStream

case class BufferedReaderIterator(reader: BufferedReader) extends Iterator[String] {
  override def hasNext() = reader.ready
  override def next() = reader.readLine()
  // override def next() = {
  //   val line = reader.readLine()
  //   closeIfDone
  //   line
  // }
  // def closeIfDone = if(hasNext) true else {reader.close(); false}
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
