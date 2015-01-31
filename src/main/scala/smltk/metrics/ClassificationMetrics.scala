package smltk.metrics

import breeze.linalg._

/* This implements several scores, losses and utility functions to measure classification
 * preformance
 *
 * @author Fayimora Femi-Balogun
 */

object ClassificationMetrics {
  /** Computes the subset accuracy: the set of label predicted for a sample must exactly match the
   * corresponding set of labels in yTrue
   *
   * @param yTrue The correct labels, A.K.A Ground Truth
   * @param yPreds The predicted labels, as returned by a classifier
   * @param normalize If false, return a count of correctly classified samples. Otherwise, return
   * the fraction(between 0 and 1) of correctly classified samples
   *
   * @throws [[java.lang.IllegalArgumentException]]
   *
   * @return the accuracy of a classifier's predictions.
   */
  def accuracy(yTrue: DenseVector[Double], yPreds: DenseVector[Double], normalize: Boolean = true): Double = {
    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    var count = 0.0
    for (i <- 0 until yTrue.size; if yTrue(i)==yPreds(i)) count += 1

    if (normalize) count/yTrue.size else count
  }
}
