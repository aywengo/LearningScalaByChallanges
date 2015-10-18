package Lesson_1.TapeEquilibrium

import Math.abs

object Solution {
  def solution(A: Array[Int]): Int = {
    val t0 = System.nanoTime()
    val result = diffTapes(A, A.sum - A(0), Int.MaxValue, 1, A(0))
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def diffTapes(A:Array[Int], S:Int, M:Int, I:Int = 1, T:Int = 0): Int ={
    if (I >= A.length) return M

    diffTapes(A, S - A(I), abs(T - S) min M, I + 1, T + A(I))
  }

}

object Test extends App {
  var nums = List(3, 1, 2, 4, 3)
  println(Solution.solution(nums.toArray))

  nums = List(10, 40, 25, 70)
  println(Solution.solution(nums.toArray))

  nums = List(3, -3)
  println(Solution.solution(nums.toArray))


  nums = List(2, 2)
  println(Solution.solution(nums.toArray))
}
