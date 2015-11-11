// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/2-CountingElements.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingX37CSP-975/

package Lesson_2.FrogRiverOne

import org.scalatest.FlatSpec

object Solution {
  def solution(X: Int, A: Array[Int]): Int = {
    val t0 = System.nanoTime()
    val fact = sumUp(X)
    val river = Array.ofDim[Boolean](X + 1)
    val result = jump(A, river, 0, fact)
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def jump(A: Array[Int], River: Array[Boolean], Sum: Int, Fact: Int, Moment: Int = 0): Int = {
    if (Moment >= A.length) return -1

    val nextJump = leaveFailCheck(A(Moment), River, Sum)
    if (nextJump == Fact) return Moment

    jump(A, River, nextJump, Fact, Moment + 1)
  }

  def leaveFailCheck(Leave: Int, River: Array[Boolean], Sum: Int): Int = {
    if (Leave >= River.length || River(Leave)) return Sum

    River(Leave) = true
    Sum + Leave
  }

  def sumUp(n: Int): Int = {
    ((n + 1) * n) / 2
  }

}

class Lesson_2_Test extends FlatSpec {
  def check(A: Int, Sut: Array[Int], Expected: Int) = {
    s"MissingInteger_${Sut.deep} with $A" should s"return expected value $Expected" in {
      assert(Solution.solution(A, Sut) === Expected)
    }
  }

  check(5, Array(1, 3, 1, 4, 2, 3, 5, 4), 6)
}
