// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/4-Sorting.pdf
//
// Results might be found under the links:
// https://codility.com/demo/results/trainingU88FVV-FZK/
// https://codility.com/demo/results/trainingUUN62M-24S/
//    -- using utils for QuickSort and diff in order to avoid BigInt casting

package Lesson_4.Triangle

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)
    val B = A.filter(_ > 0).sorted.map(BigInt(_))
    println(B.deep)

    for (a <- 2 until B.length) {
      if (B(a) < B(a - 1) + B(a - 2)) return 1
    }
    0
  }
}

class Lesson_4_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"Triangle_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }

  check(Array(10, 2, 5, 1, 8, 20), 1)
  check(Array(-10, 2, 5, -1, 8, 20), 0)
  check(Array(0, 1, 2), 0)
  check(Array(Int.MaxValue, Int.MaxValue, Int.MaxValue), 1)
  check(Array(), 0)
}