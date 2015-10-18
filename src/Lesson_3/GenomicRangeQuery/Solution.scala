package Lesson_3.GenomicRangeQuery

import util.control.Breaks._

object Solution {
  def solution(S: String, P: Array[Int], Q: Array[Int]): Array[Int] = {
    val m = aggregate(S)

    println(S)
    println(m.deep)

    val result = Array.ofDim[Int](P.length)
    for (c <- P.indices) {
      result(c) = query(m, P(c),Q(c))
    }
    result
  }

  def query(A: Array[Array[Int]], F:Int, T:Int) : Int = {
    for (c <-1 until 4) {
      breakable {
      for (d <- A(c - 1).indices)
        {
          if (A(c-1)(d) < 0 ) break
          else if (A(c-1)(d) <= T && A(c-1)(d) >= F) return c
        }
      }
    }
    4
  }

  def aggregate(S: String): Array[Array[Int]] = {
    val result = Array.fill(3,S.length)(-1)
    val pointers = Array.ofDim[Int](3)
    for (c <- 0 until S.length) {
      val x = getGenom(S, c)
      if (x < 4) {
        result(x-1)(pointers(x-1)) = c
        pointers(x-1) += 1
      }
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

object Test extends App {
  //println(Solution.solution("CAGCCTA", List(2, 5, 0).toArray, List(4, 5, 6).toArray).deep) // exp [2, 4, 1]
  //println(Solution.solution("TC", List(0, 0, 1).toArray, List(0, 1, 1).toArray).deep) // exp [4, 2, 2]
  println(Solution.solution("G", List(0).toArray, List(0).toArray).deep) // exp [3]
}