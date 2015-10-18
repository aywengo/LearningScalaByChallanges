package Lesson_3.CountDiv.Solution

object Solution {
  def solution(A: Int, B: Int, K: Int): Int = {
    if (A == B) {
      if (A % K == 0) return 1
      return 0
    }
    if (K > B) return 0
    if (K == 1) return B - A + 1
    ((B - firstDivItem(A,K)) / K) + 1
  }

  def firstDivItem(A: Int, K: Int): Int = {
    if (A % K == 0) return A
    firstDivItem(A +1, K)
  }
}

object Test extends App {
  println(Solution.solution(5, 11, 2))
  println(Solution.solution(6, 12, 2))
  println(Solution.solution(0, 0, 11))
  println(Solution.solution(1, 1, 11))
  println(Solution.solution(10, 10, 7))
  println(Solution.solution(10, 10, 5))
}
