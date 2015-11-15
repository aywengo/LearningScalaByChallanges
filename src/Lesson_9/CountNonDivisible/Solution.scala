// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/9-Sieve.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training6PH84D-MGR/  100 0
// https://codility.com/demo/results/training2FQ2D7-K7G/

package Lesson_9.CountNonDivisible

import org.scalatest._

import scala.collection.mutable

object Solution {
  def solution(A: Array[Int]): Array[Int] = {
    val max = A.max

    val divisors = mutable.Map.empty[Int,mutable.Set[Int]]
    A.foreach(f = element => divisors += element -> mutable.Set[Int](1, element))

    var divisor = 2
    while (divisor*divisor <= max) {
      var element_candidate = divisor
      while (element_candidate  <= max) {
        if (divisors.contains(element_candidate) && !divisors(element_candidate).contains(divisor)) {
          divisors(element_candidate) += divisor
          divisors(element_candidate) += Math.ceil(element_candidate/divisor).toInt
        }
        element_candidate += divisor
      }
      divisor += 1
    }

    val counters = Array.ofDim[Int](max + 1)
    A.foreach(e => counters(e) += 1)

    val result = Array.ofDim[Int](A.length)
    for (elem <- result.indices) {
      var count = 0
      divisors(A(elem)).foreach(e => count += 1 * counters(e))
      result(elem) = A.length - count
    }
    result
  }
}

class Lesson_9_Test extends FlatSpec {
  def check(A: Array[Int], Expected: Array[Int]) = {
    s"CountNonDivisible_A ${A.deep}" should s"return expected value ${Expected.deep} " in {
      assert(Solution.solution(A) === Expected)
    }
  }

  check(Array(3, 1, 2, 3, 6), Array(2, 4, 3, 2, 0))
}
