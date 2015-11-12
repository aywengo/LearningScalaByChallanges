// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/8-PrimeNumbers.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training3GDWFD-XAN/

package Lesson_8.MinPerimeterRectangle

import org.scalatest._

object Solution {
  def solution(N: Int): Int = {
    var minPerimeter = Int.MaxValue

    for (n <- 1 to math.sqrt(N.toDouble).toInt) {
      if (N % n == 0) {
        val minPerim: Int = perimeter(N, n)
        if (minPerimeter > minPerim) minPerimeter = minPerim
      }
    }
    minPerimeter
  }

  def perimeter(N: Int, A: Int): Int = {
    2 * (A + N / A)
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Int, Expected: Int) = {
    s"MinPerimeterRectangle_$Sut" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"$Sut should return $Expected"))
    }
  }

  check(30, 22)

}
