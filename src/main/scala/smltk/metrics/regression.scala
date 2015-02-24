package smltk.metrics

import breeze.linalg._
import breeze.numerics._

/** This implements several scores, losses and utility functions to measure regression preformance
 *
 * @author Fayimora Femi-Balogun
 */

object regression {
  def mae(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var mae = 0.0 + sum(abs(yTrue - yPreds))
    mae/yTrue.size
  }

  def rss(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    var rss = 0.0 + sum((yTrue - yPreds) :^ 2.0)
    rss
  }

  def mse(yTrue: DenseVector[Double], yPreds: DenseVector[Double]): Double = {
    rss(yTrue, yPreds)/yTrue.size
  }
}
