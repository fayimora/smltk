import breeze.linalg._
import smltk.linearmodel._
import smltk.metrics.ClassificationMetrics.accuracy

object Playground extends App {

  val X = DenseMatrix.rand[Double](200, 10)
  val y = DenseVector.rand[Double](200)

  val linReg = new LinearRegression()
  println (linReg.fit(X, y))
  println (linReg.score(X, y))

  val ridgeReg = new RidgeRegression(0.005)
  println (ridgeReg.fit(X, y))
  println (ridgeReg.score(X, y))

  println("========== Accuracy ==========")
  println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,7)))
  println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,12)))
  println(accuracy(DenseVector(1,2,3,4,5,6,7), DenseVector(1,2,3,4,5,6,12), normalize=false))
  // println(accuracy(DenseVector(), DenseVector()))

}
