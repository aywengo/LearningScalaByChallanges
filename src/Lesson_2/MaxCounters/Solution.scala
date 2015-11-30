// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/2-CountingElements.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingVVF6QN-GW9/

package Lesson_2.MaxCounters

import org.scalatest.FlatSpec

object Solution {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    println(A.deep)
    incByIndex(A, Array.ofDim[Int](N), N)
  }

  def incByIndex(A: Array[Int], R: Array[Int], N: Int, I: Int = 0, M: Int = 0, B: Int = 0): Array[Int] = {
    if (I >= A.length) {
      for (a <- R.indices if R(a) < B) R(a) = B
      return R
    }

    if (A(I) >= N + 1) {
      incByIndex(A, R, N, I + 1, M, M)
    }
    else {
      if (R(A(I) - 1) < B) {
        R(A(I) - 1) = B + 1
      }
      else {
        R(A(I) - 1) += 1
      }

      val r = R(A(I) - 1)
      var max = M
      if (r > M) max = r
      incByIndex(A, R, N, I + 1, max, B)
    }
  }
}

class Lesson_2_Test extends FlatSpec {
  def check(A: Int, Sut: Array[Int], Expected: Array[Int]) = {
    s"MaxCounters_${Sut.deep} with $A" should s"return expected value ${Expected.deep}" in {
      assert(Solution.solution(A, Sut) === Expected)
    }
  }

  check(1, Array(1), Array(1))
  check(4, Array(2, 3, 2, 1, 5, 1), Array(3, 2, 2, 2))
}