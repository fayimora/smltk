package smltk.linearmodel

import breeze.linalg._
import breeze.numerics._

class RidgeRegression(val lambda: Double) extends LinearModel {

  override def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = ((X.t * X) + (DenseMatrix.eye[Double](nFeats) * lambda)) \ (X.t * y)
    weights
  }

  def predict(x: Transpose[DenseVector[Double]]): Double = x * weights
}
