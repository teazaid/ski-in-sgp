package my.task

import scala.io.Source
import scala.util.Try

/**
  * Created by Alexander on 15.07.2017.
  */
object RemoteFileReader {
  type ArrayParser = (List[String] => Array[Array[Int]])

  def readFile(url: String, parser: ArrayParser): Array[Array[Int]] = {
    val source = Source.fromURL(url)
    val lines = source.getLines().toList
    source.close()
    parser(lines)
  }

}
