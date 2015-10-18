package Lesson_3.PassingCars

object Solution {
  def solution(A: Array[Int]): Int = {
    val p = countPostFixSumVector(A, Array.ofDim[Int](A.length), A.length - 1)
    sumUp(A, p, 0)
  }

  def sumUp(A: Array[Int], P: Array[Int], T: Int, I:Int = 0, S:Int = 0): Int = {
    if (I >= A.length) return S
    var sum = S
    if (A(I) == T) sum = S + P(I)
    if (sum > 1000000000) return -1
    sumUp(A, P, T, I + 1, sum)
  }

  def countPostFixSumVector(A: Array[Int], P: Array[Int], I:Int, S:Int = 0): Array[Int] =        {
    if (I < 0) return P
    val s = S + A(I)
    P(I) = s
    countPostFixSumVector(A, P, I - 1, s)
  }
}
