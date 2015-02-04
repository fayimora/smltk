package smltk.linearmodel

import breeze.linalg._
import breeze.numerics._

class LinearRegression extends LinearModel {

  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = (X.t * X) \ (X.t * y)
    weights
  }

  def predict(x: Transpose[DenseVector[Double]]): Double = x * weights
}

object LinearRegression {
  def apply() = {
    new LinearRegression()
  }
}
