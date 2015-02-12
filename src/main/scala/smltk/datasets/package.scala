package smltk

import breeze.linalg.{DenseMatrix, DenseVector}
import java.io.File
import scala.io.Source._
import smltk.utils.GZIterator

package object datasets {

  val BASE_FOLDER = "src/main/resources/datasets/"

  /** Loads the MNIST dataset.
   *
   * @param full specifies if the full 10 digits should be loaded. If false, the train data only
   * contains labels 1, 2 and 3
   *
   * @return a tuple of the pixels and corresponding labels
   */
  def loadMnist(full: Boolean = true): (DenseMatrix[Double], DenseVector[Int]) = {
    val suffix = if(full) "mnist-train.dat.gz" else "mnist123-train.dat.gz"
    val data = GZIterator(BASE_FOLDER+suffix)
    val fullMatrix = data.map(_.split("\\s+").map(_.toDouble)).toIndexedSeq
    val mat = DenseMatrix(fullMatrix:_*)
    val y = mat(::, 0).map(_.toInt)
    val X = mat(::, 1 until mat.cols)
    (X, y)
  }

  /** Loads the iris dataset found at https://archive.ics.uci.edu/ml/datasets/Iris
   *
   * @return a tuple of the data and labels. Labels are encoded as 1,2,3
   */
  def loadIris(): (DenseMatrix[Double], DenseVector[Int]) = {
    val data = fromFile(BASE_FOLDER+"iris.csv").getLines
    val X = data.map(l => l.split(",").dropRight(1).map(_.toDouble)).toArray
    val y = DenseVector((for (i <- 0 until 150) yield { if(i<50) 0 else if(i<100) 1 else 2 }):_* )
    (DenseMatrix(X:_*), y)
  }

  /** Loads the boston dataset found at https://archive.ics.uci.edu/ml/datasets/Housing
   *
   * @return a matrix of the boston dataset
   */
  def loadBoston(): DenseMatrix[Double] = {
    var data = fromFile(BASE_FOLDER+"boston.dat").getLines
    val X = data.map(l => l.trim.split(" +").map(_.toDouble)).toArray
    DenseMatrix(X:_*)
  }
}

