// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/1-TimeComplexity.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingN9KYND-Y54/

package Lesson_1.TapeEquilibrium

import java.lang.Math.abs

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    diffTapes(A, A.sum - A(0), Int.MaxValue, 1, A(0))
  }

  def diffTapes(A: Array[Int], S: Int, M: Int, I: Int = 1, T: Int = 0): Int = {
    if (I >= A.length) return M

    diffTapes(A, S - A(I), abs(T - S) min M, I + 1, T + A(I))
  }

}

class Lesson_1_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"TapeEquilibrium_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }

  check(Array(3, 1, 2, 4, 3), 1)
  check(Array(10, 40, 25, 70), 5)
  check(Array(3, -3), 6)
  check(Array(2, 2), 0)
}
