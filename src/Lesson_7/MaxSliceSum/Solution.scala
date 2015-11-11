// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/7-MaxSlice.pdf
// https://en.wikipedia.org/wiki/Maximum_subarray_problem
//
// Results might be found under link:
// https://codility.com/demo/results/trainingPMWKGJ-WED/

package Lesson_7.MaxSliceSum

import org.scalatest._

object Solution {
  def solution(A: Array[Int]): Int = {
    golden_max_slice(A)
  }

  def golden_max_slice(A: Array[Int]): Int = {
    var maxEnding, maxSlice = A(0)
    val maxItem = A.max
    for (a <- 1 until A.length) {
      maxEnding = A(a) max (maxEnding + A(a))
      maxSlice = maxSlice max maxEnding
    }
    maxSlice max maxItem
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"MaxSliceSum_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"${Sut.deep} should return $Expected"))
    }
  }

  check(Array(-2, 1, 1), 2)
  check(Array(3, 2, -6, 4, 0), 5)
  check(Array(3, 2, -6, 3, 1), 5)
  check(Array(-10), -10)
  check(Array(-2, 1), 1)
  check(Array(-2, 3), 3)
  check(Array(-2, -2), -2)
}
