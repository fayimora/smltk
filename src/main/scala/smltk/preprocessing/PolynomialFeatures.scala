class PolynomialFeatures {
  def fitTransform(data: DenseMatrix[Double]) = {
    val degree = 2
    data(*, ::).map(row => toPolyFeats(row))
  }

  private def toPolyFeats(vec: DenseVector[Double]) = {
    val feats = ArrayBuffer(1.0) ++ vec.toScalaVector
    for(i <- 0 until data.size) {
      feats += math.pow(vec(i), 2)
      for(j <- i+1 until data.size)
        feats += vec(i) * vec(j)
    }
  }
}
