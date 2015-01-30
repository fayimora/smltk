package com.fayimora.smltk

import breeze.linalg._
import Types._

trait Model {
	def fit(X: MatD, y: VecD): VecD

	def predict(x: Transpose[VecD]): Double

	def predict(X: MatD): VecD = {
		val results = Array[Double]()
		for (i <- 0 to X.rows) results :+ predict(X(i, ::))
		DenseVector[Double](results)
	}
}