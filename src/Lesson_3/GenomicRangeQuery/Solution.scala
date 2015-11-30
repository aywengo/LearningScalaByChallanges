// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/3-PrefixSums.pdf
//
// Results might be found under the link:
// https://codility.com/demo/results/trainingAT7268-SBS/

package Lesson_3.GenomicRangeQuery

import org.scalatest.{Matchers, FlatSpec}

import scala.util.control.Breaks._

object Solution {
  def solution(S: String, P: Array[Int], Q: Array[Int]): Array[Int] = {
    val m = aggregate(S)

    println(S)
    println(m.deep)

    val result = Array.ofDim[Int](P.length)
    for (c <- P.indices) {
      result(c) = query(m, P(c), Q(c))
    }
    result
  }

  def query(A: Array[Array[Int]], F: Int, T: Int): Int = {
    for (c <- 1 until 4) {
      breakable {
        for (d <- A(c - 1).indices) {
          if (A(c - 1)(d) < 0) break
          else if (A(c - 1)(d) <= T && A(c - 1)(d) >= F) return c
        }
      }
    }
    4
  }

  def aggregate(S: String): Array[Array[Int]] = {
    val result = Array.fill(3, S.length)(-1)
    val pointers = Array.ofDim[Int](3)
    for (c <- 0 until S.length if getGenom(S, c) < 4) {
      val x = getGenom(S, c)
      result(x - 1)(pointers(x - 1)) = c
      pointers(x - 1) += 1
    }
    result
  }

  def getGenom(S: String, c: Int): Int = {
    (S(c): Char) match {
      case 'A' => 1
      case 'C' => 2
      case 'G' => 3
      case 'T' => 4
    }
  }
}

class Lesson_3_Test extends FlatSpec with Matchers {
  def check(Sut: String, P: Array[Int], Q: Array[Int], Expected: Array[Int]) = {
    s"GenomicRangeQuery_$Sut with ${P.deep} and ${Q.deep}" should s"return expected value ${Expected.deep} " in {
      assert(Solution.solution(Sut, P, Q) === Expected)
    }
  }

  check("CAGCCTA", Array(2, 5, 0), Array(4, 5, 6), Array(2, 4, 1))
  check("TC", Array(0, 0, 1), Array(0, 1, 1), Array(4, 2, 2))
  check("G", Array(0), Array(0), Array(3))
}