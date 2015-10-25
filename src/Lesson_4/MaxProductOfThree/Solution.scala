package Lesson_4.MaxProductOfThree

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)

    val B = A.sorted
    B(B.length-3)*B(B.length-2)*B.last max B.head * B(1) * B.last
  }
}

object Test extends App {
  println(Solution.solution(Array(-3, 1, 2, -2, 5, 6))) // exp 60
  println(Solution.solution(Array(6, 7, 1, 2, 3, -2, 5, 4, -8))) // exp 210
  println(Solution.solution(Array(4, 5, 1, 0))) // exp 20
  println(Solution.solution(Array(-5, 5, -5, 4))) // exp 125
}