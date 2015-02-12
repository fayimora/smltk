package smltk.linearmodel

import breeze.linalg._
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer._
import breeze.numerics._

class LogisticRegression {
  var X: DenseMatrix[Double] = _
  var weights: DenseMatrix[Double] = _
  var nSamples = 0
  var nFeats = 0

  def fit(X: DenseMatrix[Double], y: DenseVector[Int], k: Int) = {
    this.X = X
    nSamples = X.rows
    nFeats = X.cols

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

    val initWeights = DenseVector.rand(k * nFeats)
    val params = OptParams()
    weights = minimize(objective, initWeights, params).toDenseMatrix.reshape(k, nFeats)
    println("returned result")
    println(weights)
  }

  def predict() = {
  }
}

object LogisticRegression {
  def apply() = {
    new LogisticRegression()
  }
}

