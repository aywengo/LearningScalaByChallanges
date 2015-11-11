// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/4-Sorting.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingTB3XSA-CKC/

package Lesson_4.MaxProductOfThree

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)

    val B = A.sorted
    B(B.length-3)*B(B.length-2)*B.last max B.head * B(1) * B.last
  }
}

class Lesson_4_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"MaxProductOfThree_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }
  check(Array(-3, 1, 2, -2, 5, 6), 60)
  check(Array(6, 7, 1, 2, 3, -2, 5, 4, -8), 210)
  check(Array(4, 5, 1, 0), 20)
  check(Array(-5, 5, -5, 4), 125)
}