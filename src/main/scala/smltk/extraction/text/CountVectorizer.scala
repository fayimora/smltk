package smltk.extraction.text

import breeze.linalg.{Counter, DenseMatrix}

class CountVectorizer(override val ngramRange: (Int, Int) = (1, 1),
                      val pattern: String = "(?u)\\b\\w\\w+\\b",
                      override val lowerCase: Boolean = true) extends Vectorizer[Int] {

  compiler = pattern.r.findAllMatchIn _

  override def fitTransform(documents: IndexedSeq[String]): DenseMatrix[Int] = {
    val uniqueTokens = toFeatures(documents).flatten.distinct
    val mat = for(doc <- documents) yield {
      val counter = Counter.countTraversable(toFeatures(doc))
      uniqueTokens.map(tok => counter(tok))
    }
    DenseMatrix(mat:_*)
  }

}

