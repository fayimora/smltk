package smltk.cluster

import breeze.linalg._

class Kmeans(val nClusters: Int,
  val tolerance: Double = 1e-5,
  val maxIterations: Int = 300,
  val nRuns: Int = 5) {

  /** The data on which we run k-means clustering */
  var X: DenseMatrix[Double] = null

  /** The number of data points */
  var nSamples = 0

  /** The number of features/dimensions */
  var nFeats = 0

  /** The cluster centers of the clusters */
  var centroids: DenseMatrix[Double] = null

  var assignments: DenseMatrix[Double] = null

  /** The corespondint cluster number of each datapoint*/
  var labels: DenseVector[Int] = null

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
          objective += math.pow(norm((clustering(j,::) - centroids(k,::)).t), 2)
      }
      lastObjective = objective
      converged = math.abs(objective - lastObjective) >= tolerance
      iteration += 1
      if(converged) println(s"Converged after ${iteration} iterations")
    }

    def assignLabels() {
      // compute distance between each data point and the centroids
      for(d <- 0 until X.rows; k <- 0 until nClusters){
        assignments(d, k) = math.pow(norm( (centroids(k,::) - X(d,::)).t ), 2)
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
   * @return the corresponding predicted labels for the data points
   *
   * TODO: Implement this!
   */
  def predict(X: DenseMatrix[Double]): DenseVector[Double] = ???

}
