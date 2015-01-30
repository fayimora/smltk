package smltk

import breeze.linalg._
import breeze.numerics._

class RidgeRegression extends LinearRegression with Model {

  override def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    // Setup instnce variables
    nSamples = X.rows
    nFeats = X.cols

    weights = ((X.t * X) + (DenseMatrix.eye[Double](nFeats) * 0.6)) \ (X.t * y)
    weights
  }
}
