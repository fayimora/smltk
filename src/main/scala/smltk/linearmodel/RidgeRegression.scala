package smltk.linearmodel

import breeze.linalg._
import breeze.numerics._

class RidgeRegression(val lambda: Double) extends LinearRegression with LinearModel {

  override def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = ((X.t * X) + (DenseMatrix.eye[Double](nFeats) * lambda)) \ (X.t * y)
    weights
  }
}
