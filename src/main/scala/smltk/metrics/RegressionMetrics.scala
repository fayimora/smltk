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
    var mae = 0.0
    for (i <- 0 until yTrue.size) mae += abs(yTrue(i) - yPreds(i))
    mae/yTrue.size
  }

  def rss_score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var rss = 0.0
    for (i <- 0 until yTrue.size) rss += math.pow(yTrue(i) - yPreds(i), 2)
    rss
  }

  def mse_score(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var mse = 0.0
    for (i <- 0 until yTrue.size) mse += math.pow(yTrue(i) - yPreds(i), 2)
    mse/yTrue.size
  }
}
