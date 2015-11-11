// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/7-MaxSlice.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingS7S66Q-PNT/

package Lesson_7.MaxProfit

import org.scalatest._

object Solution {
  def solution(A: Array[Int]): Int = {
    if (A.length < 2) return 0

    var maxProfit, maxEnding = 0
    var min = A(0)

    for(i <-1 until A.length) {
      maxEnding = 0 max A(i) - min
      min = min min A(i)
      maxProfit = maxEnding max maxProfit
    }

    maxProfit
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"MaxProfit_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"${Sut.deep} should return $Expected"))
    }
  }

  check(Array(23171, 21011, 21123, 21366, 21013, 21367), 356)
  check(Array(), 0)
  check(Array(1), 0)

}
