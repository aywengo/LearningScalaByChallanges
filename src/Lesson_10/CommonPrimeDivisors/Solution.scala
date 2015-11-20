// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/10-Gcd.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingZUUEJK-DTX/ - 85 16 Sieve Erathosfen (timeouts)
// https://codility.com/demo/results/trainingR3AFJN-AHS/

package Lesson_10.CommonPrimeDivisors

import org.scalatest._

import scala.collection.{immutable, mutable}

object Solution {
  def solution(A: Array[Int], B: Array[Int]): Int = {
    var count = 0
    for (i <- A.indices) {
      val gcd_ab = gcd(A(i), B(i))
      if (check(A(i), gcd_ab) && check(B(i), gcd_ab)) {
        count += 1
      }
    }
    count
  }

  def check(A: Int, gcd_ab: Int): Boolean = {
    var rest = A / gcd_ab
    while (gcd_ab % rest != 0) {
      val gcd_tmp = gcd(gcd_ab, rest)
      if (gcd_tmp == 1) {
        return false
      }
      rest /= gcd_tmp
    }
    true
  }

  def gcd(A: Int, B: Int): Int = {
    if (A % B == 0) B else gcd(B, A % B)
  }
}

class Lesson_10_Test extends FlatSpec {
  def check(A: Array[Int], B: Array[Int]) = {
    val Expected = sample(A, B)
    s"CommonPrimeDivisors_A ${A.deep} B ${B.deep} " should s"return expected value $Expected " in {
      assert(Solution.solution(A, B) === Expected)
    }
  }

  def sample(A: Array[Int], B: Array[Int]): Int = {
    val max = B.max max A.max

    val divisors = getDivisors(max)
    val primes = getPrimeDivisors(divisors)

    var count = 0
    for (elem <- A.indices) {
      if (checkSimilarityOfPrimeDivisorsSet(A(elem), B(elem), primes)) {
        count += 1
      }
    }
    count
  }

  def checkSimilarityOfPrimeDivisorsSet(A: Int, B: Int, Primes: mutable.Map[Int, immutable.Set[Int]]): Boolean = {
    A == B ||
      (Primes(A).nonEmpty && Primes(B).nonEmpty && Primes(A) == Primes(B))
  }

  def getPrimeDivisors(Divisors: mutable.Map[Int, mutable.Set[Int]]): mutable.Map[Int, immutable.Set[Int]] = {
    val primeDivisors = mutable.Map[Int, immutable.Set[Int]]()
    val primes = Divisors.filter(p => p._2.size == 2).keySet
    primeDivisors += 1 -> Set(1)
    for (elem <- 2 to Divisors.size) {
      primeDivisors += elem -> Divisors(elem).intersect(primes).toSet
    }
    primeDivisors
  }

  def getDivisors(Max: Int): mutable.Map[Int, mutable.Set[Int]] = {
    val divisors = mutable.Map.empty[Int, mutable.Set[Int]]
    (1 to Max).foreach(f = element => divisors += element -> mutable.Set[Int](1, element))

    var divisor = 2
    while (divisor * divisor <= Max) {
      var element_candidate = divisor
      while (element_candidate <= Max) {
        if (divisors.contains(element_candidate) && !divisors(element_candidate).contains(divisor)) {
          divisors(element_candidate) += divisor
          divisors(element_candidate) += Math.ceil(element_candidate / divisor).toInt
        }
        element_candidate += divisor
      }
      divisor += 1
    }
    divisors
  }

  check(Array(15, 10, 9), Array(75, 30, 5))
  check(Array(5), Array(25))
  check(Array(1), Array(1))
}
