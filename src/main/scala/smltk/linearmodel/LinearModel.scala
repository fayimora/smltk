package smltk.linearmodel

import breeze.linalg._

trait LinearModel {

  /** The weights of the model. This is in fact the model*/
  var weights = DenseVector[Double]()

  /** The number of samples in the dataset */
  var nSamples = 0

  /** The number of features in the dataset */
  var nFeats = 0

  /** This function computes the weights of the model
   * @param X the datam atrix
   * @param y the targets
   *
   * @return the computed weights. It is also stored as an instance variable
   */
  def fit(X: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double]

  /** This function computes the prediction of a single instance of data
   * @param x a row vector which is also a single instance
   *
   * @return a real valued number which is the prediction for the argument instance
   */
  def predict(x: Transpose[DenseVector[Double]]): Double

  /** This function computes the predictions for multiple instances
   * @param X input matrix, each row is a single instance
   *
   * @return a column vector of predicted values
   */
  def predict(X: DenseMatrix[Double]): DenseVector[Double] = {
    val results = for (i <- 0 until X.rows) yield predict(X(i, ::))
    DenseVector[Double](results.toArray)
  }

  /** This function computes the Mean Squared Error for this regressor
   *
   * @param yTrue the true values
   * @param yPreds the predictions
   *
   * @return the Mean Squared Error \sum_i (y_i - \hat{y_i})^2
   */
  def score(X: DenseMatrix[Double], yTrue: DenseVector[Double]): Double = {
    var mse = 0.0
    val yPreds = predict(X)
    for (i <- 0 until yTrue.size) mse += math.pow(yTrue(i) - yPreds(i), 2)
    mse
  }
}
