package smltk.metrics

import breeze.linalg._
import math.{abs, pow}

/** This implements several scores, losses and utility functions to measure regression
 * preformance
 *
 * @author Fayimora Femi-Balogun
 */

object RegressionMetrics {
  def mae_score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var mae = 0.0 + sum(abs(yTrue - yPreds))
    mae/yTrue.size
  }

  def rss_score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var rss = 0.0 + sum((yTrue - yPreds) :^2)
    rss
  }

  def mse_score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    rss_score(yTrue, yPreds)/yTrue.size
  }
}
