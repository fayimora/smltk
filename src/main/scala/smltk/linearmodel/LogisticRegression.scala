package smltk.linearmodel

import breeze.linalg._
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer._
import breeze.numerics._
import smltk.base.Classifier

class LogisticRegression extends Classifier {
  var X: DenseMatrix[Double] = _
  var weights: DenseMatrix[Double] = _
  var log_probabilities: DenseVector[Double] = _

  /** The number of samples in the dataset */
  // var nSamples = 0

  /** The number of features in the dataset */
  // var nFeats = 0


  def fit(X: DenseMatrix[Double], y: DenseVector[Int]) = {
    this.X = X
    nSamples = X.rows
    nFeats = X.cols
    k = y.toArray.distinct.size

    val objective = new DiffFunction[DenseVector[Double]] {
      def calculate(thetas: DenseVector[Double]) = {
        val w = thetas.toDenseMatrix.reshape(k, nFeats)
        var cost = 0.0
        for(i <- 0 until nSamples) {
          for(c <- 0 until k) {
            val a = I(y(i) == c)
            val numer = exp(w(c,::).t dot X(i, ::).t)
            val denom = (for(l <- 0 until k) yield exp(w(c,::).t dot X(i, ::).t)).sum
            cost += a * log(numer/denom)
          }
        }

        // compute gradient
        var grad = DenseVector.rand(k*nFeats)
        // var grad = DenseVector.zeros(k*nFeats)
        // for(j <- 0 until k) {
        //   var sum = DenseVector.zeros[Double](nFeats)
        //   for (i <- 0 to nSamples) {
        //     val numer = exp(w(j,::).t dot X(i, ::).t)
        //     val denom = (for(l <- 0 until k) yield exp(w(j,::).t dot X(i, ::).t)).sum
        //     val p = numer/denom
        //     val diff = I(y(i)==j) - p
        //     sum += X(i, ::).t * diff
        //   }
        //   grad(j, ::) := (-1/m) * sum
        // }

        println(cost)

        // no way this is efficient!
        // this.X.t.toArray.sliding(r, r).zipWithIndex.filter(t => ids.contains(t._2)).toArray
        // mat.t.toArray.sliding(3,3).map(_.mkString("::")).toArray

        (-cost, grad)
      }
    }

    // val initWeights = DenseVector.zeros[Double](k * nFeats)
    val initWeights = DenseVector.rand(k * nFeats)
    val params = OptParams(tolerance = 1E-6)
    weights = minimize(objective, initWeights, params).toDenseMatrix.reshape(k, nFeats)
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

