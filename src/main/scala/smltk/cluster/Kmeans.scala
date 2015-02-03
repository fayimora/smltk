package smltk.cluster

import breeze.linalg._
import breeze.linalg.functions.euclideanDistance

/** Clustering is done using <a href="http://en.wikipedia.org/wiki/Lloyd%27s_algorithm">Lloyd's
 * algorithm</a>. Worst case complexity of the algorithm is O(n^(k/p))
 *
 * @param nClusters the number of clusters to search for
 * @param tolerance the relative tolerance with regards to inertia used to declare convergence
 * @param maxIterations the maximum number of iterations in a single run. This helps avoid problems
 * that don't converge
 * @param nRuns number of times k-means will be run, each with different centroids. The final result
 * of k-means will be the run that best minises the objective
 */
class Kmeans(val nClusters: Int,
  val tolerance: Double = 1e-5,
  val maxIterations: Int = 300,
  val nRuns: Int = 5) {

  /** The data on which we run k-means clustering */
  var X: DenseMatrix[Double] = _

  /** The number of data points */
  var nSamples = 0

  /** The number of features/dimensions */
  var nFeats = 0

  /** The cluster centers of the clusters */
  var centroids: DenseMatrix[Double] = _

  var assignments: DenseMatrix[Double] = _
  val distanceFunc = euclideanDistance

  /** The corespondint cluster number of each datapoint*/
  var labels: DenseVector[Int] = _

  /** The value of the objective function Minimising this number is the aim of k-means */
  var objective = 0.0

  private[this] type RunResult = (Double, DenseMatrix[Double], DenseMatrix[Double], DenseVector[Int])

  /** This function computes k-mens clustering.
   * @param X the data points to be clustered
   */
  def fit(X: DenseMatrix[Double]) = {
    require(nClusters <= X.rows, "number of data points must be <= to number of clusters")
    this.X = X
    nSamples = X.rows
    nFeats = X.cols

    def getCentroids(): DenseMatrix[Double] = {
      var initCents = DenseMatrix.zeros[Double](nClusters, X.cols)
      val idxs: Array[Int] = scala.util.Random.shuffle((0 until X.rows).toList).toArray.take(nClusters)
      for (i <- 0 until nClusters) initCents(i, ::) := X(idxs(i), ::)
      initCents
    }

    val runResults: Seq[RunResult] =
      (0 until nRuns).map(_ => computeCentroids(getCentroids()))

    val (o, c, a, l) = runResults.sortBy(_._1).head
    this.objective = o
    this.centroids = c
    this.assignments = a
    this.labels = l
  }

  private def computeCentroids(initCentroids: DenseMatrix[Double]): RunResult = {
    // returns (objective, centroids, assignments, labels)
    var iteration = 0
    var lastObjective = Double.PositiveInfinity
    val centroids = initCentroids
    val assignments = DenseMatrix.zeros[Double](X.rows, nClusters)
    val labels = DenseVector.zeros[Int](X.rows)
    var converged = false

    while (iteration <= maxIterations && !converged) {
      var objective = 0.0
      assignLabels()
      maximizeParameters()

      for(k <- 0 until nClusters) {
        val clustering: DenseMatrix[Double] = pointsBelongingTo(k)
        for(j <- 0 until clustering.rows)
          objective += math.pow(distanceFunc(clustering(j,::).t, centroids(k,::).t), 2)
      }
      lastObjective = objective
      converged = math.abs(objective - lastObjective) >= tolerance
      iteration += 1
      if(converged) println(s"Converged after ${iteration} iterations")
    }

    def assignLabels() {
      // compute distance between each data point and the centroids
      for(d <- 0 until X.rows; k <- 0 until nClusters){
        assignments(d, k) = math.pow(distanceFunc(centroids(k,::).t, X(d,::).t ), 2)
        // assign allegiances
        val temp = assignments(d, ::)
        labels(d) = argmin(temp)
      }
    }

    def maximizeParameters() {
      import breeze.stats.mean
      for (k <- 0 until nClusters)
        centroids(k, ::) := mean(pointsBelongingTo(k), Axis._0).toDenseVector.t
    }

    def pointsBelongingTo(k: Int): DenseMatrix[Double] = {
      val idxs = labels.findAll(_ == k)
      val points = X(idxs, ::).toDenseMatrix
      points
    }

    (lastObjective, centroids, assignments, labels)
  }

  /** Assigns the argument datapoints to clusters
   *
   * @param X the data points for which we want to assign to clusters
   *
   * @return the corresponding predicted cluster for the data points. It does not return the actual
   * value of the centroid but the cluster index.
   */
  def predict(X: DenseMatrix[Double]): DenseVector[Int] = {
    import scala.collection.mutable.ArrayBuffer
    val labels = ArrayBuffer.empty[Int]
    X(*, ::).map { x =>
      val distances = centroids(*, ::).map( centroid => math.pow(distanceFunc(x, centroid), 2))
      labels += argmin(distances)
    }
    DenseVector[Int](labels:_*)
  }

}
