package smltk.extraction.text

import breeze.linalg.{Counter, DenseMatrix}
import breeze.numerics.log
import scala.collection.mutable.ArrayBuffer

class TfIdfVectorizer(override val ngramRange: (Int, Int) = (1, 1),
                      val pattern: String = "(?u)\\b\\w\\w+\\b",
                      override val lowerCase: Boolean = true) extends Vectorizer[Double] {

  compiler = pattern.r.findAllMatchIn _

  // TODO: find a way to make sure this works
  override def fitTransform(documents: IndexedSeq[String]): DenseMatrix[Double] = {
    val uniqueTokens = toFeatures(documents).flatten.distinct
    val tf = ArrayBuffer.empty[IndexedSeq[Double]]
    val idf = ArrayBuffer.empty[IndexedSeq[Double]]
    val counters = ArrayBuffer.empty[Counter[String, Int]]

    for(doc <- documents) {
      val counter = Counter.countTraversable(toFeatures(doc))
      counters += counter
      tf += uniqueTokens.map(tok => counter(tok)+0.0)
    }

    for(doc <- documents)
      idf += uniqueTokens.map(tok => documents.size / (1.0 + counters.count(c => c(tok) != 0)))

    val tfIdf = (DenseMatrix(tf:_*)) :* log(DenseMatrix(idf:_*))
    tfIdf
  }

}

