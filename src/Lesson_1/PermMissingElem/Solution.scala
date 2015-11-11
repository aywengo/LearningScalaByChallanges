// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/1-TimeComplexity.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingUDB9VV-BX2/

package Lesson_1.PermMissingElem

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    val length = A.length
    val fact = ((length.toLong + 2) * (length + 1))/2

    val t0 = System.nanoTime()
    val result = (fact - A.sum).toInt
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

class Lesson_1_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"PermMissingElem_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }
  check(Array(2,3,1,5), 4)
}
