package smltk.metrics.crossvalidation


class KFold(val nSamples: Int, val k: Int) {
  def getIndices(): Seq[(Vector[Vector[Int]], Vector[Int])] = {
    val idxs = (0 until nSamples).toVector.grouped(nSamples/k).toVector
    for(i <- 0 until k)
      yield (idxs.slice(0, i) ++ idxs.slice(i+1, idxs.size), idxs(i))
  }
}

object KFold {
  def apply(nSamples: Int, k: Int = 5) = {
    new KFold(nSamples, k).getIndices
  }
}
