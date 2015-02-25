package smltk.naivebayes

import breeze.linalg._
import smltk.base.Classifier

trait NaiveBayes extends Classifier {
  private val ZERO_PROBABILITY = 1e-12
  var probabilities = None
}

class MultinomialNB extends NaiveBayes {
  def fit(X: DenseMatrix[Double],y: DenseVector[Int]) = ???
  def predict(x: Transpose[DenseVector[Double]]): Int = ???
}

class GaussianNB extends NaiveBayes {
  def fit(X: DenseMatrix[Double],y: DenseVector[Int]) = ???
  def predict(x: Transpose[DenseVector[Double]]): Int = ???
}

class BernoulliNB extends NaiveBayes {
  def fit(X: DenseMatrix[Double],y: DenseVector[Int]) = ???
  def predict(x: Transpose[DenseVector[Double]]): Int = ???
}

