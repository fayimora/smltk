package smltk.metrics

import breeze.linalg._

/** This implements several scores, losses and utility functions to measure classification
 * preformance
 *
 * @author Fayimora Femi-Balogun
 */

object classification {
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
  def accuracy(yTrue: DenseVector[Int], yPreds: DenseVector[Int], normalize: Boolean = true): Double = {
    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    var count = 0.0
    for (i <- 0 until yTrue.size; if yTrue(i)==yPreds(i)) count += 1

    if (normalize) count/yTrue.size else count
  }

  /** Computes the precision, also known as, positive predictive value.
   * tp / (tp + fp)
   *
   * @param yTrue The correct labels, A.K.A Ground Truth
   * @param yPreds The predicted labels, as returned by a classifier
   *
   * @return the precision of a classifier's predictions.
   */
  def precision(yTrue: DenseVector[Int], yPreds: DenseVector[Int],
    posLabel: Int = 1): Double = {

    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    var (tp, tn, fp, fn) = confusions(yTrue, yPreds, posLabel=posLabel)
    tp / (tp+fp)
  }

  /** Computes the recall which is tp / (tp + fn)
   *
   * @param yTrue The correct labels, A.K.A Ground Truth
   * @param yPreds The predicted labels, as returned by a classifier
   *
   * @return the recall of a classifier's predictions.
   */
  def recall(yTrue: DenseVector[Int], yPreds: DenseVector[Int],
    posLabel: Int = 1): Double = {

    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    var (tp, tn, fp, fn) = confusions(yTrue, yPreds, posLabel=posLabel)
    tp / (tp+fn)
  }

  /** Computes the F1 Measure which is a harmonic mean between the precision and recall
   *
   * 2 * (precision + recall) / (precision * recall)
   *
   * @param yTrue The correct labels, A.K.A Ground Truth
   * @param yPreds The predicted labels, as returned by a classifier
   *
   * @return the recall of a classifier's predictions.
   */
  def fMeasure(yTrue: DenseVector[Int], yPreds: DenseVector[Int],
    posLabel: Int = 1): Double = {

    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    val p = precision(yTrue, yPreds)
    val r = recall(yTrue, yPreds)
    2 * ( (p*r) / (p+r) )

  }

  /* Return the true positives, true negatives, false positives and false negatives counts */
  private def confusions(yTrue: DenseVector[Int], yPreds: DenseVector[Int],
    posLabel: Int = 1): (Double, Double, Double, Double) = {

    require(yTrue.size == yPreds.size, "Both vectors must be of the same length")
    require(yTrue.size > 0, "Vector must not be empty")

    var (tp, tn, fp, fn) = (0, 0, 0, 0)
    yTrue.toArray.zip(yPreds.toArray).foreach{ case (gold, pred) =>
      if(gold == posLabel)
        if(pred == gold) tp += 1 // true positive
        else fn += 1 // false negative
      else
        if(pred == gold) tn += 1 // true negative
        else fp += 1 // false positive
    }
    (tp, tn, fp, fn)
  }
}
