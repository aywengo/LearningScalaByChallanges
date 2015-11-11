// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/1-TimeComplexity.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingUSN7Z3-UTS/

package Lesson_1.FrogJmp

import org.scalatest.FlatSpec

object Solution {
  def solution(X: Int, Y: Int, D: Int): Int = {
    BigDecimal((Y - X) / D.toDouble).setScale(0, BigDecimal.RoundingMode.UP).toInt
  }
}

class Lesson_1_Test extends FlatSpec {
  def check(X: Int, Y: Int, D: Int, Expected: Int) = {
    s"FrogJmp $X $Y $D" should s"return expected value $Expected " in {
      assert(Solution.solution(X, Y, D) == Expected)
    }
  }

  check(1, 5, 2, 2)
}
