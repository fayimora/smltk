package smltk.extraction.text

import java.io._
import collection.mutable.HashMap
import breeze.linalg.{norm, DenseVector}
import smltk.utils.BinaryReader

class Word2Vec {
  val vocab = HashMap[String, DenseVector[Double]]()
  private var numTokens = 0
  private var dimension = 0

  /** This function loads a trained model(binary format)
   *
   *  @param filePath the full path to the binary file
   *  @param limit maximum number of tokens to load
   */
  def load(filePath: String, limit: Integer = Int.MaxValue) {
    val file = new File(filePath)
    if (!file.exists())
      throw new FileNotFoundException("Binary vector file not found <" + file.toString + ">")

    // Create new reader to read data
    val reader = new BinaryReader(file)

    // Read header info
    numTokens = reader.readToken().toInt
    dimension = reader.readToken().toInt

    // Read the vocab tokens and their associated vector representations
    var token = ""
    val vector = new Array[Double](dimension)
    var normFactor = 1f
    for (_ <- 0 until math.min(numTokens, limit)) {
      // read the token and vector
      token = reader.readToken()
      for (i <- 0 until vector.length) vector(i) = reader.readDouble()

      // Store the normalized vector representation, keyed by the token
      val normalisedVec = vector.map(_ / normFactor)
      vocab.put(token, DenseVector(normalisedVec:_*))

      // Eat up the next delimiter character
      reader.read()
    }
    reader.close()
  }

  /** function used to access the vector of a token.
   *  val model = Word2Vec()
   *  model.load("/path/to/bin/file")
   *  model("people") // returns vector of the argument token
   *
   *  @param token the token for which we want a vector representation
   *
   *  @return the vector representation of the token
   */
  def apply(token: String) = {
    require(contains(token), s"Out of vocabulary word: $token")
    vocab(token)
  }

  /** Checks if the token is present in the vocabulary.
   *
   * @param word Token to be checked.
   *
   * @return True if the word is in the vocab map.
   */
  def contains(word: String): Boolean = {
    vocab.get(word).isDefined
  }

  /** Returns the cosine similarity between two strings. This operation is commutative. It is
   *  expected that the argument tokens are in the vocabulary of the trained model.
   *  @param token1 the first token
   *  @param token2 the second token
   *
   *  @return the cosine similarity
   */
  def cosine(token1: String, token2: String): Double = {
    cosine(apply(token1), apply(token2))
  }

  /** Returns the cosine similarity between two vectors. This operation is commutative
   *  @param v1 the first vector
   *  @param v2 the second vector
   *
   *  @return the cosine similarity
   */
  def cosine(v1: DenseVector[Double], v2: DenseVector[Double]): Double = {
    (v1 dot v2) / (norm(v1) * norm(v2))
  }

}
