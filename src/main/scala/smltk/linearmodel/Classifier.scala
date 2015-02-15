package smltk.linearmodel

import breeze.linalg._

trait Classifier {

  /** The number of samples in the dataset */
  var nSamples = 0

  /** The number of features in the dataset */
  var nFeats = 0

  var k = 0

  /** This function computes the weights of the model
   * @param X the data matrix
   * @param y the targets
   *
   * @return the computed weights. It is also stored as an instance variable
   */
  def fit(X: DenseMatrix[Double], y: DenseVector[Int])

  def predict(x: Transpose[DenseVector[Double]]): Int

  def predict(x: DenseVector[Double]): Int = predict(x.t)

  def predict(X: DenseMatrix[Double]): DenseVector[Int] =
    X(*, ::).map(x => predict(x))
}
