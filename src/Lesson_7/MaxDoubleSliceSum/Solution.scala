// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/7-MaxSlice.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training6CV9TG-VNX/

package Lesson_7.MaxDoubleSliceSum

import org.scalatest._

object Solution {
  def solution(A: Array[Int]): Int = {
    val K1, K2 = Array.ofDim[Int](A.length)

    for (i <- 1 until A.length - 1) {
      K1(i) = 0 max (K1(i - 1) + A(i))
    }

    for (i <- A.length - 2 until 0 by -1) {
      K2(i) = 0 max (K2(i + 1) + A(i))
    }

    var maximum = 0
    for (i <- 1 until A.length - 1) {
      maximum = maximum max (K1(i - 1) + K2(i + 1))
    }

    maximum
  }
}

class Lesson_7_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"MaxDoubleSliceSum_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"${Sut.deep} should return $Expected"))
    }
  }

  check(Array(3, 2, 6, -1, 4, 5, -1, 2), 17)
}
