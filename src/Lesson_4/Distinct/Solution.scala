package Lesson_4.Distinct

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)

    A.groupBy(x => x).keys.size
  }
}

object Test extends App {
  println(Solution.solution(Array(-5, 5, -5, 4))) // exp 2
  println(Solution.solution(Array(0, 1, 2))) // exp 3
  println(Solution.solution(Array())) // exp 0
}