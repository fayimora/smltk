package smltk.extraction.text

import java.io._
import collection.mutable.HashMap
import breeze.linalg.DenseVector
import smltk.utils.BinaryReader

class Word2Vec {
  val vocab = HashMap[String, DenseVector[Double]]()
  private var numTokens = 0
  private var dimension = 0


  def load(filename: String, limit: Integer = Int.MaxValue) {
    val file = new File(filename)
    if (!file.exists())
      throw new FileNotFoundException("Binary vector file not found <" + file.toString + ">")

    // Create new reader to read data
    val reader = new BinaryReader(file)

    // Read header info
    // numTokens = Integer.parseInt(reader.readToken())
    // dimension = Integer.parseInt(reader.readToken())
    numTokens = reader.readToken().toInt
    dimension = reader.readToken().toInt

    // Read the vocab words and their associated vector representations
    var word = ""
    val vector = new Array[Double](dimension)
    var normFactor = 1f
    for (_ <- 0 until math.min(numTokens, limit)) {
      // Read the word
      word = reader.readToken()

      // Read the vector representation (each vector contains dimension number of floats)
      for (i <- 0 until vector.length) vector(i) = reader.readDouble()

      // Store the normalized vector representation, keyed by the word
      val normalisedVec = vector.map(_ / normFactor)
      vocab.put(word, DenseVector(normalisedVec:_*))

      // Eat up the next delimiter character
      reader.read()
    }
    // println("Loaded " + math.min(numTokens, limit) + " words.\n")
    reader.close()
  }

}
