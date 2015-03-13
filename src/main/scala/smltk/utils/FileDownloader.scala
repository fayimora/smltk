package smltk.utils

import sys.process._
import java.net.URL
import java.io.File
import scala.util.{Try, Success, Failure}

object FileDownloader {
  def apply(url: String, filename: String): Boolean = {
    val open = Try(new URL(url) #> new File(filename) !!)
    open map (v => true) getOrElse false
  }
}
