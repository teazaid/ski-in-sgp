package my.task

import scala.util.Try

/**
  * Created by Alexander on 17.07.2017.
  */
object DataParser {
  def parse(lines: List[String]): Array[Array[Int]] = {
    val indexedLines = lines.zipWithIndex
    val arraySizeOpt = indexedLines.find { case (line, index) => index == 0 }

    arraySizeOpt.flatMap { case (i, j) =>
      val sizes = i.split(" ")
      val arrayContent = indexedLines.drop(1)
      Try(parseArrayData(arrayContent, parseSize(sizes(0)), parseSize(sizes(1)))).toOption
    }.getOrElse(Array.empty)
  }

  private def parseSize(sizeStr: String): Int = {
    Try(sizeStr.toInt).getOrElse(0)
  }

  private def parseArrayData(data: List[(String, Int)], rows: Int, columns: Int): Array[Array[Int]] = {
    val array = Array.ofDim[Int](rows, columns)
    for {
      (line, index) <- data
    } yield {
      val elements = line.split(" ")
      (0 until elements.length).foreach { j =>
        array(index - 1)(j) = elements(j).toInt
      }
    }
    array
  }
}
