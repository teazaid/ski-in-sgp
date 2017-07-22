package my.task

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Alexander on 17.07.2017.
  */
class DataParserSpec extends FlatSpec with Matchers {
  private val expectedResult = Array(
    Array(4, 8, 7, 3),
    Array(2, 5, 9, 3),
    Array(6, 3, 2, 5),
    Array(4, 4, 1, 6)
  )

  "DataParser" should "parse correct structure" in {
    val actualResult = DataParser.parse(
      List("4 4",
        "4 8 7 3",
        "2 5 9 3",
        "6 3 2 5",
        "4 4 1 6")
    )
    actualResult should be(expectedResult)
  }

  "DataParser" should "return none if failed to parse" in {
    val actualResult = DataParser.parse(
      List("asda",
        "long test")
    )
    actualResult should be(Array.empty)

  }
}
