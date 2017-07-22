package my.task

import my.task.solver.TaskSolver

/**
  * Created by Alexander on 16.07.2017.
  */
object Runner {
  def main(args: Array[String]) {
    val data = RemoteFileReader.readFile("http://s3-ap-southeast-1.amazonaws.com/geeks.redmart.com/coding-problems/map.txt",
      DataParser.parse)

    val start = System.currentTimeMillis()
    val bestPath = TaskSolver.solve(data)
    println(s"time: ${System.currentTimeMillis() - start}")

    println(s"result ${bestPath}")
    println(s"Steep lvl ${bestPath.head - bestPath.last}")
    println(s"Longest Path ${bestPath.size}")
  }
}