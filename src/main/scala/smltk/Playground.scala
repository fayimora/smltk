import breeze.linalg._
import smltk.linearmodel._

object Playground extends App {

  val X = DenseMatrix.rand[Double](200, 10)
  val y = DenseVector.rand[Double](200)

  val linReg = new LinearRegression()
  println (linReg.fit(X, y))
  println (linReg.score(X, y))

  val ridgeReg = new RidgeRegression(0.005)
  println (ridgeReg.fit(X, y))
  println (ridgeReg.score(X, y))

}
