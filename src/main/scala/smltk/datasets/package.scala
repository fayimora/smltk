package smltk

import breeze.linalg.{DenseMatrix, DenseVector}
import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import java.io.File
import scala.io.Source._

package object datasets {

  val BASE_FOLDER = "src/main/resources/datasets/"

  /** Loads the iris dataset found at https://archive.ics.uci.edu/ml/datasets/Iris
   *
   * @return a tuple of the data and labels. Labels are encoded as 1,2,3
   */
  def loadIris(): (DenseMatrix[Double], DenseVector[Int]) = {
    val data = fromFile(BASE_FOLDER+"iris.csv").getLines
    val X = data.map(l => l.split(",").dropRight(1).map(_.toDouble)).toArray
    val y = DenseVector((for (i <- 0 until 150) yield { if(i<50) 1 else if(i<100) 2 else 3 }):_* )
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

