package smltk

import breeze.linalg.{Tensor, DenseMatrix, DenseVector, Transpose}

object Types {

	// Basic convenience Vector and Matrix types
	// Still contemplating whether I want to use stuff like this
	type VecD = DenseVector[Double]
	type VecI = DenseVector[Int]
	type MatD = DenseMatrix[Double]
	type MatI = DenseMatrix[Int]
}
