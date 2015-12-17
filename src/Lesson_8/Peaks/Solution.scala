// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/8-PrimeNumbers.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training42UCAS-DP3/

package Lesson_8.Peaks

import org.scalatest._

import scala.collection.mutable.ListBuffer

object Solution {
  def solution(A: Array[Int]): Int = {
    if (A.length < 3) return 0

    var peaks = ListBuffer[Int]()
    for (i <- 1 until A.length - 1 if A(i - 1) < A(i) && A(i + 1) < A(i)) {
      peaks += i
    }

    val n = peaks.length min (A.length - 1) / 2
    for (i <- n to 1 by -1 if A.length % i == 0) {
      var a = 0
      val chunkSize = A.length / i
      for (p <- peaks if p >= a * chunkSize && p < (a + 1) * chunkSize) {
        a += 1
        if (a == i) return a
      }
    }
    0
  }
}

class Lesson_8_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"Peaks_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"${Sut.deep} should return $Expected"))
    }
  }

  check(Array(1, 2, 3, 4, 3, 4, 1, 2, 3, 4, 6, 2), 3)
  check(Array(1, 2, 3), 0)
  check(Array(1, 2), 0)
  check(Array(1), 0)
  check(Array(), 0)

}
