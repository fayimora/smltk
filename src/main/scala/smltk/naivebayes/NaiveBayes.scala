package smltk.naivebayes

import breeze.linalg._

trait NaiveBayes {
  private val ZERO_PROBABILITY = 1e-12
  var probabilities = None

  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): Unit

  def predict(X: DenseMatrix[Double]): DenseVector[Double]

  def score(Xtest: DenseMatrix[Double], yTrue: DenseVector[Double]): Double = {
    val yPreds = predict(Xtest)
    import smltk.metrics.ClassificationMetrics.accuracy
    accuracy(yTrue, yPreds)
  }
}
