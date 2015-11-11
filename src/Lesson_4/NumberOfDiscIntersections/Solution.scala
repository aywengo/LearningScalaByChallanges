// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/4-Sorting.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingJE7Z8V-RT3/

package Lesson_4.NumberOfDiscIntersections

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    val maxIterations = 10000000
    val downLimits, upLimits = Array.ofDim[Int](A.length)
    for (a <- A.indices) {
      if (A(a) >= A.length) {
        downLimits(0) += 1
        upLimits(A.length - 1) += 1
      } else {
        if (A(a) + a >= A.length) {
          upLimits(A.length - 1) += 1
        } else {
          upLimits(a + A(a)) += 1
        }
        if (a - A(a) < 0) {
          downLimits(0) += 1
        } else {
          downLimits(a - A(a)) += 1
        }
      }
    }
    println(downLimits.deep)
    println(upLimits.deep)
    var result, inRange = 0

    for (i <- downLimits.indices) {
      inRange += downLimits(i)

      if (inRange > 0 && upLimits(i) != 0) {
        result += sumIntersects(upLimits, inRange, i)
        if (result > maxIterations) {
          return -1
        }
        inRange -= upLimits(i)
      }
    }

    result
  }

  def sumIntersects(UpLimits: Array[Int], InRange: Int, id: Int): Int = {
    UpLimits(id) * (2 * InRange - 1 - UpLimits(id)) / 2
  }
}

class Lesson_4_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"NumberOfDiscIntersections_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }

  check(Array(1, 5, 2, 1, 4, 0), 11)
}