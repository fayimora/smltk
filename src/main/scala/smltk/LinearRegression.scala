package smltk

import breeze.linalg._
import breeze.numerics._

class LinearRegression extends Model {

  var weights = DenseVector[Double]()
  var nSamples = 0
  var nFeats = 0

  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = (X.t * X) \ (X.t * y)
    weights
  }

  def predict(x: Transpose[DenseVector[Double]]): Double = {
    x * weights
  }

  /** This function returns the Mean Squared Error for this regressor
   *
   * @param yTrue the true values
   * @param yPreds the predictions
   *
   * @return the Mean Squared Error
   */
  def score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var mse = 0.0
    for (i <- 0 to yTrue.size) mse += math.pow(yTrue(i) - yPreds(i), 2)
    mse
  }
}
