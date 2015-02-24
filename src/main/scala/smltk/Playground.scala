import breeze.linalg._
import smltk.linearmodel._
import smltk.cluster._
import smltk.neighbours.KNeighboursClassifier
import smltk.metrics.classification._
import smltk.metrics.crossvalidation.KFold
import smltk.datasets._
import smltk.extraction.text._

object Playground {

  def main(args: Array[String]) = {
    word2vec()
    // tfIdfVectorizer()
    // countVectorizer()
    // logistic()
    // linearmodels()
    // crossvalidation()
    // knn()
    // metrics()
    // clustering()
  }

  def word2vec() {
    val model = new Word2Vec()
    model.load("/Users/fayimora/Misc/word2vec-scala/vectors.bin")
    println(model.vocab.get("people"))
  }

  def tfIdfVectorizer() {
    val vect = new TfIdfVectorizer(ngramRange=(1, 2))
    val corpus = IndexedSeq("my name is fayi", "her name is wene", "i love my siblings",
      "wene certainly loves me", "i love her too")
    println(vect.toFeatures(corpus))
    println(vect.fitTransform(corpus))
  }

  def countVectorizer() {
    val vect = new CountVectorizer(ngramRange=(1, 3))
    val corpus = IndexedSeq("my name is fayi", "her name is wene", "i love my siblings",
      "wene certainly loves me", "i love her too")
    println(vect.toFeatures(corpus))
    println(vect.fitTransform(corpus))
  }

  def logistic() {
    val data = mnist()
    val X = data._1
    val y = data._2
    val clf = LogisticRegression()
    clf.fit(X, y)
  }

  def crossvalidation(){
    val kf = KFold(14, k=3, shuffle=true)
    kf.map { tuple =>
      val (train, test) = (tuple._1, tuple._2)
      println(s"Train Indices: ${train.mkString(", ")}")
      println(s"Test Indices: ${test.mkString(", ")}")
      println()
    }
  }

  def knn() {
    val data = iris()
    val X = data._1
    val y = data._2
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
    val ridgeReg = LinearRegression(0.001)
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
    val data = iris()
    val X = data._1
    val y = data._2
    val pca = princomp(X)
    // val model = pca.loadings(0 until 2, ::)
    // val XNew = (model.t * model) * pca.center

    val cl = Kmeans(3, maxIterations=1000, nRuns=5)
    cl.fit(pca.scores)
    println(cl.centroids)
    println(s"Objective: ${cl.objective}")
    println(cl.labels)
  }
}
