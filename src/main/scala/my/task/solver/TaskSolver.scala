package my.task.solver

/**
  * Created by Alexander on 16.07.2017.
  */
object TaskSolver {

  case class Point(x: Int, y: Int)
  case class FindPathResult(bestPath: List[Int], cache: CacheResult)
  type CacheResult = Map[Point, List[Int]]
  //type FindPathResult = (List[Int], CacheResult)

  private val InitFindPathResult = FindPathResult(List.empty[Int], Map.empty[Point, List[Int]])

  def solve(data: Array[Array[Int]]): List[Int] = {
    val coordinates = for {
      i <- (0 until data.length)
      j <- (0 until data(i).length)
    } yield (i, j)

    val bestFindPathResult = coordinates.foldLeft(InitFindPathResult) { case (initFindPathResult, (i, j)) =>
      val newFindPathResult = findPath(i, j, data, initFindPathResult.cache)
      val longestFindPathResult = reduce(Seq(initFindPathResult.bestPath, newFindPathResult.bestPath))
      FindPathResult(longestFindPathResult, newFindPathResult.cache)
    }

    bestFindPathResult.bestPath
  }

  private def reduce(dataToReduce: Seq[List[Int]]): List[Int] = dataToReduce.fold(List.empty[Int]) {
    (collectionFirst, collectionSecond) =>
      val collectionFirstSize = collectionFirst.size
      val collectionSecondSize = collectionSecond.size

      if (collectionFirstSize < collectionSecondSize) {
        collectionSecond
      } else if (collectionFirstSize == collectionSecondSize && collectionSecondSize > 0) {
        findSteeper(collectionFirst, collectionSecond)
      } else collectionFirst
  }

  private def findSteeper(pathFirst: List[Int], pathSecond: List[Int]): List[Int] = {
    val (pathFirstSteepLvl, pathSecondSteepLvl) = (pathFirst, pathSecond) match {
      case (pathFirstHead :: _ :: _, pathSecondHead :: _ :: _) =>
        val pathFirstSteepLvl = math.abs(pathFirstHead - pathFirst.last)
        val pathSecondSteepLvl = math.abs(pathSecondHead - pathSecond.last)
        (pathFirstSteepLvl, pathSecondSteepLvl)
      case (collectionFirstHead :: Nil, collectionSecondHead :: Nil) =>
        (collectionFirstHead, collectionSecondHead)
    }

    findSteeper(pathFirst, pathSecond, pathFirstSteepLvl, pathSecondSteepLvl)
  }

  private def findSteeper(pathFirst: List[Int], pathSecond: List[Int], pathFirstSteepLvl: Int, pathSecondSteepLvl: Int): List[Int] = {
    if (pathSecondSteepLvl > pathFirstSteepLvl)
      pathSecond
    else
      pathFirst
  }

  private def findPath(i: Int, j: Int, data: Array[Array[Int]], cache: CacheResult): FindPathResult = {

    val currentPosition = data(i)(j)
    cache.get(Point(i, j)).map { cachedResult => FindPathResult(cachedResult, cache) }.getOrElse {
      // left
      val moveLeftPredicate = j - 1 >= 0 && data(i)(j - 1) < currentPosition
      val leftPathResult = move(i, j - 1, moveLeftPredicate, data, currentPosition, cache)

      val moveRightPredicate = j + 1 < data.length && data(i)(j + 1) < currentPosition
      val rightPathResult = move(i, j + 1, moveRightPredicate, data, currentPosition, leftPathResult.cache)

      // bottom
      val moveDownPredicate = i + 1 < data.length && data(i + 1)(j) < currentPosition
      val downPathResult = move(i + 1, j, moveDownPredicate, data, currentPosition, rightPathResult.cache)

      // up
      val moveUpPredicate = i - 1 >= 0 && data(i - 1)(j) < currentPosition
      val upPathResult = move(i - 1, j, moveUpPredicate, data, currentPosition, downPathResult.cache)

      val bestPath = reduce(Seq(leftPathResult.bestPath, rightPathResult.bestPath, downPathResult.bestPath, upPathResult.bestPath))

      FindPathResult(bestPath, upPathResult.cache + (Point(i, j) -> bestPath))
    }
  }

  private def move(i: Int, j: Int,
                   predicate: Boolean,
                   data: Array[Array[Int]],
                   currentPosition: Int,
                   cachedResults: CacheResult): FindPathResult = {
    if (predicate) {
      val processResult = findPath(i, j, data, cachedResults)
      FindPathResult(currentPosition :: processResult.bestPath, processResult.cache)
    } else FindPathResult(currentPosition :: Nil, cachedResults)
  }
}
