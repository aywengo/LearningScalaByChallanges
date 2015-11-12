// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/8-PrimeNumbers.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingSHC2G9-B3S/

package Lesson_8.CountFactors

import org.scalatest._

object Solution {
  def solution(N: Int): Int = {
    var count = 0
    val sqrtOfN = math.sqrt(N.toDouble).toInt

    for (n <- 1 to sqrtOfN) {
      if (N % n == 0) {
        if (n == sqrtOfN && n * n == N) count += 1 else count += 2
      }
    }

    count
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Int, Expected: Int) = {
    s"CountFactors_$Sut" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"$Sut should return $Expected"))
    }
  }

  check(25, 3)
  check(24, 8)

}
