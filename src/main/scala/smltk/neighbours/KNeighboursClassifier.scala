package smltk.neighbours

import breeze.linalg._
import breeze.linalg.functions.euclideanDistance

class KNeighboursClassifier(val nNeighbours: Int = 5){
  var X: DenseMatrix[Double] = _
  var y: DenseVector[Int] = _
  val distanceFunc = euclideanDistance

  /** Fits the data. We simply just remember everything
   *
   * @param X The dataset
   * @param y The corresponding labels of the dataset
   *
   */
  def fit(X: DenseMatrix[Double], y: DenseVector[Int]) = {
    require(X.rows > nNeighbours, "The number of instances must be greater than K")
    this.X = X
    this.y = y
  }

  /** This function predicts what class the argument data point belongs to.
   *
   * @param x the data point for which we want to determine its class
   *
   */
  def predict(x: DenseVector[Double]): Int = {
    // compute distance between each point and other points
    val distances = for(i <- 0 until this.X.rows) yield distanceFunc(this.X(i, ::).t, x)

    // take the top k distances which are the k smallest distances
    val topKDistances = distances.sorted.take(nNeighbours)

    // grab the indices for these distances
    val idxs = topKDistances.map(distances indexOf)

    // get votes for each possible label of these points
    val votes = idxs.map(i => y(i)).groupBy(n => n).map(tup => (tup._1, tup._2.size))

    // The label with highest votes wins. TODO: what happens when there is a draw?
    votes.maxBy(tup => tup._2)._1
  }

  /** This function computes the classes of each datapoint in the argument matrix.
   *
   * @param X the data points
   *
   */
  def predict(X: DenseMatrix[Double]): DenseVector[Int] =
    X(*, ::).map( x => predict(x))

}

object KNeighboursClassifier {
  def apply(nNeighbours: Int=5) = {
    new KNeighboursClassifier(nNeighbours)
  }
}
