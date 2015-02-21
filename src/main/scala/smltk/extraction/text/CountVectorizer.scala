package smltk.extraction.text

import breeze.linalg.{Counter, DenseVector, DenseMatrix}

class CountVectorizer(val ngramRange: (Int, Int) = (1, 1),
                      val pattern: String = "(?u)\\b\\w\\w+\\b") {

  val compile = pattern.r.findAllMatchIn _

  def fit_transform(documents: IndexedSeq[String]): DenseMatrix[Int] = {
    val uniqueTokens = toFeatures(documents).flatten.distinct
    val mat = for(doc <- documents) yield {
      val counter = Counter.countTraversable(toFeatures(doc))
      uniqueTokens.map(tok => counter(tok))
    }
    DenseMatrix(mat:_*)
  }

  def toFeatures(sequence: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    for(seq <- sequence) yield toFeatures(seq)
  }

  def toFeatures(str: String): IndexedSeq[String] = {
    val tokenised = compile(str).map(_.toString).toIndexedSeq
    val lsOfLists = for(i <- ngramRange._1 to ngramRange._2) yield tokenised.sliding(i).toIndexedSeq
    lsOfLists.flatten.map(_.mkString(" "))
  }

}

