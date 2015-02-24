package smltk.linearmodel

import breeze.linalg._

trait Regressor {

  /** The weights of the model. This is in fact the model*/
  var weights = DenseVector[Double]()

  /** The number of samples in the dataset */
  var nSamples = 0

  /** The number of features in the dataset */
  var nFeats = 0

  /** This function computes the weights of the model
   * @param X the data matrix
   * @param y the targets
   *
   * @return the computed weights. It is also stored as an instance variable
   */
  def fit(X: DenseMatrix[Double], y: DenseVector[Double])

  /** This function computes the prediction of a single instance of data
   * @param x a row vector which is also a single instance
   *
   * @return a real valued number which is the prediction for the argument instance
   */
  def predict(x: Transpose[DenseVector[Double]]): Double
  def predict(x: DenseVector[Double]): Double = predict(x.t)

  /** This function computes the predictions for multiple instances
   * @param X input matrix, each row is a single instance
   *
   * @return a column vector of predicted values
   */
  def predict(X: DenseMatrix[Double]): DenseVector[Double] = {
    X(*, ::).map(x => predict(x.t))
    // val results = for (i <- 0 until X.rows) yield predict(X(i, ::))
    // DenseVector[Double](results)
  }

  /** This function computes the Residual Sum of Squared Errors for this regressor
   *
   * @param X the test dataset
   * @param yTrue the true values
   *
   * @return the residual sum of squares \sum_i (y_i - \hat{y_i})^2
   */
  def score(X: DenseMatrix[Double], yTrue: DenseVector[Double]): Double = {
    import smltk.metrics.regression.rss
    val yPreds = predict(X)
    rss(yTrue, yPreds)
  }
}
