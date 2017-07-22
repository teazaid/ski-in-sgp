package my.task.solver

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Alexander on 17.07.2017.
  */
class TaskSolverSpec extends FlatSpec with Matchers {
  private val InputData = Array(
    Array(4, 8, 7, 3),
    Array(2, 5, 9, 3),
    Array(6, 3, 2, 5),
    Array(4, 4, 1, 6)
  )

  "TaskSolver" should "find longest path" in {
    TaskSolver.solve(InputData) should be(List(9, 5, 3, 2, 1))

    TaskSolver.solve(Array(Array(5))) should be(List(5))

    TaskSolver.solve(Array(
      Array(7, 9),
      Array(6, 1)
    )) should be(List(9, 7, 6, 1))

    TaskSolver.solve(Array(
      Array(7, 8),
      Array(7, 7)
    )) should be(List(8, 7))

    TaskSolver.solve(Array(
      Array(7, 7),
      Array(7, 7)
    )) should be(List(7))

    TaskSolver.solve(Array.empty[Array[Int]]) should be(Array.empty)
  }
}
