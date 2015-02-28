package smltk.extraction.text

import java.io._
import collection.mutable.HashMap
import breeze.linalg.DenseVector
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

}
