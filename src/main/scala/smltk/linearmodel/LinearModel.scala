package smltk.linearmodel

import breeze.linalg._

trait LinearModel {
  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double]

  def predict(x: Transpose[DenseVector[Double]]): Double

  def predict(X: DenseMatrix[Double]): DenseVector[Double] = {
    val results = for (i <- 0 until X.rows) yield predict(X(i, ::))
    DenseVector[Double](results.toArray)
  }

  /** This function returns the Mean Squared Error for this regressor
   *
   * @param yTrue the true values
   * @param yPreds the predictions
   *
   * @return the Mean Squared Error
   */
  def score(X: DenseMatrix[Double], yTrue: DenseVector[Double]): Double = {
    var mse = 0.0
    // for (i <- 0 to yTrue.size) mse += math.pow(yTrue(i) - yPreds(i), 2)
    val yPreds = predict(X)
    for (i <- 0 until yTrue.size) mse += math.pow(yTrue(i) - yPreds(i), 2)
    mse
  }
}
