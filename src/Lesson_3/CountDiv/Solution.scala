// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/3-PrefixSums.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/training7MV6KP-9KJ/

package Lesson_3.CountDiv

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Int, B: Int, K: Int): Int = {
    if (A == B) {
      if (A % K == 0) return 1
      return 0
    }
    if (K > B) return 0
    if (K == 1) return B - A + 1
    ((B - firstDivItem(A, K)) / K) + 1
  }

  def firstDivItem(A: Int, K: Int): Int = {
    if (A % K == 0) return A
    firstDivItem(A + 1, K)
  }
}

class Lesson_3_Test extends FlatSpec {
  def check(A: Int, B: Int, K: Int, Expected: Int) = {
    s"CountDiv_$A with $B and $K" should s"return expected value $Expected " in {
      assert(Solution.solution(A, B, K) === Expected)
    }
  }

  check(5, 11, 2, 3)
  check(6, 12, 2, 4)
  check(0, 0, 11, 1)
  check(1, 1, 11, 0)
  check(10, 10, 7, 0)
  check(10, 10, 5, 1)
}
