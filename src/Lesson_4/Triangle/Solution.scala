package Lesson_4.Triangle

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)
    val B = A.filter(_ > 0).filter(_ < A.length).sorted.map(BigInt(_))
    println(B.deep)

    var r = 0
    for(a <-2 until B.length) {
      if (B(a) < B(a-1) + B(a-2)) r += 1
    }
    r
  }
}

object Test extends App {
  println(Solution.solution(Array(10, 2, 5, 1, 8, 20))) // exp 1
  println(Solution.solution(Array(-10, 2, 5, -1, 8, 20))) // exp 0
  println(Solution.solution(Array(0, 1, 2))) // exp 0
  println(Solution.solution(Array(Int.MaxValue, Int.MaxValue, Int.MaxValue))) // exp 1
  println(Solution.solution(Array())) // exp 0
}