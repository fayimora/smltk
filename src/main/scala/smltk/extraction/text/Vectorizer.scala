package smltk.extraction.text

import breeze.linalg.{DenseMatrix}
import scala.util.matching.Regex.Match

trait Vectorizer[T] {

  var compiler: CharSequence => Iterator[Match] = _
  val ngramRange = (1, 1)
  val lowerCase = true

  def fitTransform(documents: IndexedSeq[String]): DenseMatrix[T]

  def toFeatures(sequence: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    for(seq <- sequence) yield toFeatures(seq)
  }

  def toFeatures(str: String): IndexedSeq[String] = {
    val tokenised = compiler(str).map{ s =>
      if(lowerCase) s.toString.toLowerCase else s.toString
    }.toIndexedSeq
    val lsOfLists = for(i <- ngramRange._1 to ngramRange._2) yield tokenised.sliding(i).toIndexedSeq
    lsOfLists.flatten.map(_.mkString(" "))
  }

}

