package smltk.metrics

import breeze.linalg._
import breeze.numerics._

/** This implements several scores, losses and utility functions to measure regression preformance
 *
 * @author Fayimora Femi-Balogun
 */

object regression {

  /** Compute the Mean Absolute Error. This is generally used to measure how
   *  close predictions are to eventual outcomes.
   *  @param yTrue the true predictions
   *  @param yPreds the model predictions
   *
   * @return the residual sum of squared errors
   */
  def mae(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var mae = 0.0 + sum(abs(yTrue - yPreds))
    mae/yTrue.size
  }

  /** Compute the Residual Sum of Squares
   *  @param yTrue the true predictions
   *  @param yPreds the model predictions
   *
   * @return the residual sum of squared errors
   */
  def rss(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var rss = 0.0 + sum((yTrue - yPreds) :^ 2.0)
    rss
  }

  /** Compute the Mean Squared Error. This is simply the average residual sum of squares
   *  @param yTrue the true predictions
   *  @param yPreds the model predictions
   *
   * @return the mean squared error
   */
  def mse(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    rss(yTrue, yPreds)/yTrue.size
  }
}
