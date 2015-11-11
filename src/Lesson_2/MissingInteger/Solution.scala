// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/2-CountingElements.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingWAMM85-XDN/

package Lesson_2.MissingInteger

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    println(A.deep)

    val check = Array.ofDim[Boolean](countPositive(A))
    checkPerm(A, check)
  }

  def countPositive(A: Array[Int], I: Int = 0, C: Int = 0): Int = {
    if (I >= A.length) return C + 1
    var count = C
    if (A(I) > 0) count = C + 1
    countPositive(A, I + 1, count)
  }

  def checkPerm(A: Array[Int], C: Array[Boolean], I: Int = 0): Int = {
    if (I >= A.length) return getFirstPositive(C)

    if (A(I) > 0 && A(I) < C.length && !C(A(I))) C(A(I)) = true

    checkPerm(A, C, I + 1)
  }

  def getFirstPositive(C: Array[Boolean], I: Int = 1): Int = {
    if (I >= C.length || !C(I)) return I

    getFirstPositive(C, I + 1)
  }

}

class Lesson_2_Test extends FlatSpec {
  def check(Sut: Array[Int], Expected: Int) = {
    s"MissingInteger_${Sut.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected)
    }
  }

  check(Array(-2147483648, 2147483647), 1)
  check(Array(1), 2)
  check(Array(2, 3, 1, 5), 4)
  check(Array(-2, 2, 3, 1, 5, 6, 8), 4)
  check(Array(1, 3, 6, 4, 1, 2), 5)
}
