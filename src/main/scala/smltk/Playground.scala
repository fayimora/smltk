import breeze.linalg._
import smltk.linearmodel._
import smltk.cluster._
import smltk.neighbours.KNeighboursClassifier
import smltk.metrics.ClassificationMetrics._
import smltk.metrics.crossvalidation.KFold

object Playground {

  def main(args: Array[String]) = {
    linearmodels()
  }

  def crossvalidation(){
    val kf = KFold(14, k=3, shuffle=true)
    kf.getIndices.map { tuple =>
      val (train, test) = (tuple._1, tuple._2)
      println(s"Train Indices: ${train.mkString(", ")}")
      println(s"Test Indices: ${test.mkString(", ")}")
      println()
    }
  }

  def knn() {
    val X = loadIris()
    val y = DenseVector((for (i <- 0 until 150) yield { if(i<50) 1 else if(i<100) 2 else 3 }):_* )
    val neigh = KNeighboursClassifier(nNeighbours=20)
    neigh.fit(X, y)
    println(neigh.predict(X(0 until 150, ::)))
  }

  def linearmodels() {
    val N = 200
    val D = 10
    val X = DenseMatrix.rand[Double](N, D)
    val y = DenseVector.rand[Double](N)
    val linReg = LinearRegression()
    println (linReg.fit(X, y))
    println (linReg.score(X, y))

    println("COmputing y with Ridge Regression")
    val ridgeReg = RidgeRegression(0.001)
    println (ridgeReg.fit(X, y))
    println (ridgeReg.score(X, y))

  }
  def metrics() {
    println("========== Accuracy ==========")
    println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,7)))
    println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,12)))
    println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,12), normalize=false))
    // println(accuracy(DenseVector(), DenseVector()))
    val yTrue = DenseVector(1,1,1,0,0,0,1)
    val yPreds = DenseVector(1,1,1,1,0,0,1)
    // var (tp, tn, fp, fn) = confusions(yTrue, yPreds)
    // println("******************")
    // println(accuracy(yTrue, yPreds))
    // var acc  = (tp+tn) / (tp+tn+fp+fn)
    // println(acc)
    // println("******************")
    //
  }

  def clustering() {
    println("==================== CLustering ====================")
    // X = DenseMatrix.rand[Double](200, 10)
    val X = loadIris()
    val pca = princomp(X)
    // val model = pca.loadings(0 until 2, ::)
    // val XNew = (model.t * model) * pca.center

    val cl = Kmeans(3, maxIterations=1000, nRuns=5)
    cl.fit(pca.scores)
    println(cl.centroids)
    println(s"Objective: ${cl.objective}")
    println(cl.labels)
  }

  def loadIris(): DenseMatrix[Double] = {
    import com.github.tototoshi.csv._
    import java.io.File
    val reader = CSVReader.open(new File("/Users/fayimora/Misc/iris.csv"))
    val ls = reader.all.map(_.dropRight(1).map(_.toDouble).toArray)
    DenseMatrix(ls:_*)
  }
}
