package smltk.linearmodel

import breeze.linalg._
import breeze.numerics._

class LinearRegression extends LinearModel {

  /** This functions trains the model by computing the weights.
   *
   * @param X the input data
   * @param y the output vector
   */
  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = (X.t * X) \ (X.t * y)
    weights
  }

  /** Computes the pedicted output for a single instance.
   *
   * @param x the instance vector or which we want to comput it's output
   *
   * @return the predicted value for the given input instance
   */
  def predict(x: Transpose[DenseVector[Double]]): Double = x * weights
}

object LinearRegression {
  def apply() = {
    new LinearRegression()
  }
}
