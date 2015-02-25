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
        var grad = DenseVector.rand(k*nFeats)
        for(i <- 0 until nSamples) {
          var lhs = 0.0
          var rhs = 0.0
          for(c <- 0 until k) {
            val phi = w(c, ::).t dot X(i, ::).t
            lhs += I(y(i) == c) * phi
            rhs += math.exp(phi)
          }
          cost += lhs - log(rhs)
        }
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

