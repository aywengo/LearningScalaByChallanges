// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/9-Sieve.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingCPH5CU-6AK/

package Lesson_9.CountSemiprimes

import org.scalatest._

object Solution {
  def solution(N: Int, P: Array[Int], Q: Array[Int]): Array[Int] = {
    val result = Array.ofDim[Int](P.length)
    val semPrefSum = getSemPrefSumArray(N)
    for (e <- P.indices) {
      result(e) = semPrefSum(Q(e)) -  semPrefSum(P(e)-1)
    }

    result
  }

  def getSemPrefSumArray(N: Int): Array[Int] = {
    val factors, semCount = Array.ofDim[Int](N + 1)
    var i = 2
    while (i * i <= N) {
      if (factors(i) == 0) {
        var k = i * i
        while (k <= N) {
          if (factors(k) == 0) factors(k) = i
          k += i
        }
      }
      i += 1
    }
    for(x <- 1 until semCount.length) {
      val sem = factorization(x, factors, semCount)
      if (sem == 2) semCount(x) = semCount(x-1) + 1 else semCount(x) = semCount(x-1)
    }
    semCount
  }

  def factorization(X:Int, F:Array[Int], R:Array[Int]): Int = {
    if (F(X) > 0) {
      var primeFactors = 1
      var x = X
      while (F(x) > 0 && primeFactors <= 2) {
        if (R(x) > 0) return primeFactors + R(x)
        primeFactors += 1
        x /= F(x)
      }
      return primeFactors
    }
    0
  }
}

class Lesson_9_Test extends FlatSpec {
  def check(N: Int, P: Array[Int], Q: Array[Int], Expected: Array[Int]) = {
    s"CountSemiprimes_N $N P ${P.deep} Q ${Q.deep}" should s"return expected value ${Expected.deep} " in {
      assert(Solution.solution(N, P, Q) === Expected)
    }
  }

  check(26, Array(1, 4, 16), Array(26, 10, 20), Array(10, 4, 0))

}
