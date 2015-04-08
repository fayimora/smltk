package smltk.linearmodel

import breeze.linalg._
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer._
import breeze.numerics._
import smltk.base.Classifier

class LogisticRegression extends Classifier {
  var X: DenseMatrix[Double] = _
  var y: DenseVector[Int] = _
  var weights: DenseMatrix[Double] = _
  var log_probabilities: DenseVector[Double] = _
  var nClasses = 0


  private def getGroundTruth(): DenseMatrix[Double] = {
    // do one-hot encoding here
    val groundTruth = DenseMatrix.zeros[Double](nClasses, nSamples)
    for(k <- 0 until nClasses) {
      val idxs = y.findAll(_ == k)
      val gt = DenseVector.zeros[Double](nSamples)
      idxs.foreach{ i => gt(i) = 1}
      groundTruth(k, ::) := gt.t
    }
    groundTruth
  }

  def fit(X: DenseMatrix[Double], y: DenseVector[Int]) = {
    // setup instance variables
    this.X = X
    this.y = y
    this.nSamples = X.rows
    this.nFeats = X.cols
    this.nClasses = y.toArray.distinct.size

    val objective = new DiffFunction[DenseVector[Double]] {
      def calculate(thetas: DenseVector[Double]) = {
        val w = thetas.toDenseMatrix.reshape(nClasses, nFeats)
        val groundTruth = getGroundTruth()
        val hypothesis = w * X.t
        val probs = exp(hypothesis) / sum(exp(hypothesis))
        val costExamples = groundTruth * log(probs.t)

        // compute cost
        val cost = -(sum(costExamples) / nSamples)
        // cost += (sum(w*w.t)*0.8) # regularisation

        // compute gradient
        val grad: DenseMatrix[Double] = - ((groundTruth - probs) * X) :/ nSamples.toDouble

        println(s"Cost is: $cost")

        (cost, grad.t.toDenseVector)
      }
    }

    val initWeights = DenseVector.rand(nClasses * nFeats) :* 0.005
    val params = OptParams(tolerance = 1E-6, maxIterations = 1000, useStochastic = true)
    weights = minimize(objective, initWeights, params).toDenseMatrix.reshape(nClasses, nFeats)
    println("returned result")
    println(weights)
  }

  override def predict(x: Transpose[DenseVector[Double]]): Int = ???

  // def log_probabilities(x: Transpose[DenseVector[Double]]): Double = ???
  // def log_probabilities(x: DenseVector[Double]): Double = ???
  // def log_probabilities(X: DenseMatrix[Double]): Double = ???
}

object LogisticRegression {
  def apply() = {
    new LogisticRegression()
  }
}

