package smltk.extraction.text

import breeze.linalg.{DenseMatrix}

trait Vectorizer[T] {

  type Compiler = CharSequence => Iterator[scala.util.matching.Regex.Match]
  var compiler: Compiler = _
  val ngramRange = (1, 1)

  def fit_transform(documents: IndexedSeq[String]): DenseMatrix[T]

  def toFeatures(sequence: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    for(seq <- sequence) yield toFeatures(seq)
  }

  def toFeatures(str: String): IndexedSeq[String] = {
    val tokenised = compiler(str).map(_.toString).toIndexedSeq
    val lsOfLists = for(i <- ngramRange._1 to ngramRange._2) yield tokenised.sliding(i).toIndexedSeq
    lsOfLists.flatten.map(_.mkString(" "))
  }

}
