package smltk.naivebayes

import breeze.linalg._
import smltk.base.Classifier

trait NaiveBayes extends Classifier {
  private val ZERO_PROBABILITY = 1e-12
  var probabilities = None
}

class MultinomialNB extends NaiveBayes {
  def fit(X: breeze.linalg.DenseMatrix[Double],y: breeze.linalg.DenseVector[Int]): Unit = ???
  def predict(x: breeze.linalg.Transpose[breeze.linalg.DenseVector[Double]]): Int = ???
}

class GaussianNB extends NaiveBayes {
  def fit(X: breeze.linalg.DenseMatrix[Double],y: breeze.linalg.DenseVector[Int]): Unit = ???
  def predict(x: breeze.linalg.Transpose[breeze.linalg.DenseVector[Double]]): Int = ???
}

class BernoulliNB extends NaiveBayes {
  def fit(X: breeze.linalg.DenseMatrix[Double],y: breeze.linalg.DenseVector[Int]): Unit = ???
  def predict(x: breeze.linalg.Transpose[breeze.linalg.DenseVector[Double]]): Int = ???
}

