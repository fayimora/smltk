package smltk.naivebayes

import breeze.linalg._

trait NaiveBayes {
  private val ZERO_PROBABILITY = 1e-12
  var probabilities = None

  def fit(X: DenseMatrix[Double], y: DenseVector[Int]): Unit

  def predict(X: DenseMatrix[Double]): DenseVector[Int]

  def score(Xtest: DenseMatrix[Double], yTrue: DenseVector[Int]): Double = {
    val yPreds = predict(Xtest)
    import smltk.metrics.ClassificationMetrics.accuracy
    accuracy(yTrue, yPreds)
  }
}
