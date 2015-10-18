package Lesson_3.MinAvgTwoSlice

import scala.math.abs

object Solution {
  def solution(A: Array[Int]): Int = {
    var perfSumArray = Array.ofDim[Int](A.length)
    val pSumArray = Array.ofDim[Double](A.length)
    perfSumArray(0) = A(0)
    pSumArray(0) = 0
    perfSumArray = prefSum(A, perfSumArray)
//    val minIndex = modSum(A, perfSumArray, pSumArray)

    println(A.deep)
    println(perfSumArray.deep)
//    println(minIndex)
    println(findSliceMinVPosition(perfSumArray))
    findMinSlice(A, perfSumArray, Min = perfSumArray(1).toDouble/2)
  }

  def findMinSlice(A:Array[Int], P:Array[Int], I:Int = 0, Min:Double = Double.MaxValue, MinIndex:Int = 0, K:Int = 2): Int = {
    if(I + K > A.length) return MinIndex

    var k = K
    var i = I
    if (K == 3) {
      k = 2
      i = I + 1
    } else k = 3
    var minIndex = MinIndex
    var min = Min
    var pref = 0
    if (I > 0) pref = P(I - 1)

    val minimum = (P(I+K-1) - pref).toDouble/K

    if (minimum < Min) {
      min = minimum
      minIndex = I
    }

    findMinSlice(A, P, i, min, minIndex, k)
  }

  def prefSum(A: Array[Int], P: Array[Int], I:Int = 1): Array[Int] = {
    if (I >= A.length) return P
    P(I) = P(I-1) + A(I)
    prefSum(A, P, I+1)
  }

  def modSum(A: Array[Int], P: Array[Int], D: Array[Double], I:Int = 1, Index:Int = 0): Int = {
    if (I >= A.length) {
      println(D.deep)
      return Index
    }
    D(I) = abs(A(I).toDouble/P(I-1))
    var i = Index
    if (D(I) < D(Index)) i = I
    modSum(A, P, D, I+1, i)
  }

  def findSliceMinVPosition(P: Array[Int], I:Int = 0, E:Int = 1, MP:Int = 0, M:Double = Double.MaxValue): Int = {
    if (I >= P.length - 1) return MP
    if (E >= P.length) return findSliceMinVPosition(P, I+1, I+2, MP, M)

    var pref = 0
    var minimum = M
    var minSliceStart = MP
    if (I > 0) pref = P(I - 1)
    val min = (P(E) - pref).toDouble/(E-I+1)

    if ( min < M ) {
      minimum = min
      minSliceStart = I
    }
    findSliceMinVPosition(P, I, E+1, minSliceStart, minimum)
  }

}

object Test extends App {
//  println(Solution.solution(Array(4, 2,  2,  5,  1,  5, 8))) // exp. 1
//  println(Solution.solution(Array(5, 6, 3, 4, 9))) // exp 2
  println(Solution.solution(Array(-3, -5, -8, -4, -10))) // exp 2
  println(Solution.solution(Array(2, 3, 1, 5))) // exp 0
  println(Solution.solution(Array(12,  13, 10,  3, 2, 3, 1, 4, 2))) // exp 4
}