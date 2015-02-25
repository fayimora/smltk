package smltk.linearmodel

import breeze.linalg._
import breeze.numerics._
import smltk.base.Regressor

class LinearRegression(lambda: Double = 0.0) extends Regressor {

  def fit(X: DenseMatrix[Double], y: DenseVector[Double]) = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = ((X.t * X) + (DenseMatrix.eye[Double](nFeats) * lambda)) \ (X.t * y)
    weights
  }

  def predict(x: Transpose[DenseVector[Double]]): Double = x * weights
}

object LinearRegression {
  def apply(lambda: Double = 0.0) = {
    new LinearRegression()
  }
}
