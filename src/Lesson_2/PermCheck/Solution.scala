// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/2-CountingElements.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingT7RRY2-5YR/

package Lesson_2.PermCheck

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    val t0 = System.nanoTime()
    val sumUp = ((A.length + 1) * A.length) / 2

    println(A.deep)
    val check = Array.ofDim[Boolean](A.length + 1)
    val result = checkPerm(A, sumUp, check)

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def checkPerm(A: Array[Int], ActS:Int, C:Array[Boolean], S:Int = 0, I:Int = 0): Int = {
    if (I >= A.length) {
      if (S == ActS) return 1
      return 0
    }

    if (A(I) > A.length) return 0

    if (C(A(I))) return 0
    C(A(I)) = true

    checkPerm(A, ActS, C, S + A(I), I + 1)
  }

}

class Lesson_2_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"PermCheck_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }
  check(Array(1,3,4,2,5), 1)
  check(Array(1,4,2,5), 0)
  check(Array(9, 5, 7, 3, 2, 7, 3, 1, 10, 8), 0)
  check(Array(1,4,1), 0)
}
