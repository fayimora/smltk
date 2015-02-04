package smltk.neighbours

import breeze.linalg._
import breeze.linalg.functions.euclideanDistance

class KNeighboursClassifier(val nNeighbours: Int = 5){
  var X: DenseMatrix[Double] = _
  var y: DenseVector[Int] = _
  val distanceFunc = euclideanDistance

  def fit(X: DenseMatrix[Double], y: DenseVector[Int]) = {
    require(X.rows > nNeighbours, "The number of instances must be greater than K")
    this.X = X
    this.y = y
  }

  def predict(X: DenseMatrix[Double]): DenseVector[Int] = {
    // import scala.collection.mutable.ArrayBuffer
    // val labels = ArrayBuffer.empty[Int]
    val labels = for(i <- 0 until X.rows) yield {
      // compute distance between each point and other points
      val distances = for(j <- 0 until X.rows) yield distanceFunc(this.X(i,::).t, X(j,::).t)
      // take the top k distances which are the k smallest distances
      val topKDistances = distances.sorted.take(nNeighbours)
      // grab the indices for these distances
      val idxs = topKDistances.map(distances indexOf)
      // get votes for each possible label of these points
      val freqCounts = idxs.map(i => y(i)).groupBy(n => n).map(tup => (tup._1, tup._2.size))
      // The label with highest votes wins. TODO: what happens when there is a draw?
      // labels += freqCounts.maxBy(tup => tup._2)._1
      freqCounts.maxBy(tup => tup._2)._1
    }
    DenseVector[Int](labels:_*)
  }
}

object KNeighboursClassifier {
  def apply(nNeighbours: Int=5) = {
    new KNeighboursClassifier(nNeighbours)
  }
}
