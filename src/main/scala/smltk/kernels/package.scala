import breeze.linalg._

package object kernels {
  def polynomialKernel(degree: Int)(x: DenseVector[Double], y: DenseVector[Double]) = {
    math.pow(((x dot y) + 1), degree)
  }

  def gaussianKernel(sigma: Int)(x: DenseVector[Double], y: DenseVector[Double]) = {
  }
}
