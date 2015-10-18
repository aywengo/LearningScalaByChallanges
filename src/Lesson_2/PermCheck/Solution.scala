package Lesson_2.PermCheck

object Solution {
  def solution(A: Array[Int]): Int = {
    val t0 = System.nanoTime()
    val sumUp = ((A.length + 1) * A.length) / 2

    println(A.deep)
    val check = Array.ofDim[Boolean](A.length + 1)
    val result = checkPerm(A, sumUp, check)

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def checkPerm(A: Array[Int], ActS:Int, C:Array[Boolean], S:Int = 0, I:Int = 0): Int = {
    if (I >= A.length) {
      if (S == ActS) return 1
      return 0
    }

    if (A(I) > A.length) return 0

    if (C(A(I))) return 0
    C(A(I)) = true

    checkPerm(A, ActS, C, S + A(I), I + 1)
  }

}

object Test extends App {
  println(Solution.solution(Array(1,3,4,2,5)))
  println(Solution.solution(Array(1,4,2,5)))
  println(Solution.solution(Array(9, 5, 7, 3, 2, 7, 3, 1, 10, 8)))
  println(Solution.solution(Array(1,4,1)))
}
