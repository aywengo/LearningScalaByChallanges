// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/10-Gcd.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training4DYMHT-7KV/

package Lesson_10.ChocolatesByNumbers

import org.scalatest._

object Solution {
  def solution(N: Int, M: Int): Int = {
    N / gcd(N, M)
  }
  def gcd(A: Int, B:Int): Int = {
    if (A % B == 0) B else gcd(B, A % B)
  }
}

class Lesson_10_Test extends FlatSpec {
  def check(N: Int, M: Int, Expected: Int) = {
    s"ChocolatesByNumbers_N $N M $M " should s"return expected value $Expected " in {
      assert(Solution.solution(N, M) === Expected)
    }
  }

  check(10, 4, 5)
  check(11, 4, 11)
}
