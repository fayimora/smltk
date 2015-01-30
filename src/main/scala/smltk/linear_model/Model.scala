package smltk

import breeze.linalg._
// import Types._

trait Model {
  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double]

  def predict(x: Transpose[DenseVector[Double]]): Double

  def predict(X: DenseMatrix[Double]): DenseVector[Double] = {
    val results = Array[Double]()
    for (i <- 0 to X.rows) results :+ predict(X(i, ::))
    DenseVector[Double](results)
  }
}
