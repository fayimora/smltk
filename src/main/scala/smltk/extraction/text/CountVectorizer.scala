package smltk.extraction.text

class CountVectorizer(val ngramRange: (Int, Int) = (1, 1),
                      val pattern: String = "(?u)\\b\\w\\w+\\b") {

  val compile = pattern.r.findAllMatchIn _

  def fit() = {
  }

  def toFeatures(sequence: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    for(seq <- sequence) yield toFeatures(seq)
  }

  def toFeatures(str: String): IndexedSeq[String] = {
    compile(str).map(_.toString).toIndexedSeq
  }

}

