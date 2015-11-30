// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/8-PrimeNumbers.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training4PRRZQ-37F/

package Lesson_8.Flags

import org.scalatest._

import scala.collection.mutable.ListBuffer

object Solution {
  def solution(A: Array[Int]): Int = {
    if (A.length < 3) return 0

    var peaks = ListBuffer[Int]()
    for (i <- 1 until A.length - 1 if A(i - 1) < A(i) && A(i + 1) < A(i)) {
      peaks += i
    }

    if (peaks.isEmpty) return 0
    else if (peaks.length < 3) return peaks.length

    val maxFlagsAmount = math.ceil(math.sqrt(peaks.last - peaks.head)).toInt

    var maxFlags = 0
    for (p <- 3 to maxFlagsAmount) {
      maxFlags = maxFlags max getFlagsCount(peaks.toArray, p)
    }
    maxFlags
  }

  def getFlagsCount(P: Array[Int], K: Int): Int = {
    var count, nextFlagIndex = 1
    var lastFlagIndex = 0

    while (nextFlagIndex < P.length) {
      if (Math.abs(P(nextFlagIndex) - P(lastFlagIndex)) >= K) {
        count += 1
        if (count >= K ) return K
        lastFlagIndex = nextFlagIndex
      }
      nextFlagIndex += 1
    }
    count
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"Peaks_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"${Sut.deep} should return $Expected"))
    }
  }

  check(Array(1, 5, 3, 4, 3, 4, 1, 2, 3, 4, 6, 2), 3)
  check(Array(1, 2, 3), 0)
  check(Array(1, 2), 0)
  check(Array(1), 0)
  check(Array(), 0)

}