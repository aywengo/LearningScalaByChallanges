// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/4-Sorting.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingAAM94U-UYW/

package Lesson_4.Distinct

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)

    A.groupBy(x => x).keys.size
  }
}

class Lesson_4_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"Distinct_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }
  check(Array(-5, 5, -5, 4), 3)
  check(Array(0, 1, 2), 3)
  check(Array(), 0)
}