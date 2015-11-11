// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/6-Leader.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingGDWMAE-URH/

package Lesson_6.EquiLeader

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    val leader = goldenLeader(A)

    val count = A.count(a => a == leader)
    if (count <= A.length / 2) return 0

    var equiLeaderCount, leadersCount = 0
    for (i <- A.indices) {
      if (A(i) == leader) leadersCount += 1

      if ((leadersCount > (i + 1) / 2) && ((count - leadersCount) > ((A.length - i - 1) / 2))) equiLeaderCount += 1
    }

    equiLeaderCount
  }

  def goldenLeader(A: Array[Int]): Int = {
    var size, value, count, leader = 0
    for (k <- A.indices) {
      if (size == 0) {
        size += 1
        value = A(k)
      }
      else {
        if (value != A(k)) size -= 1 else size += 1
      }
    }

    var candidate = -1
    if (size > 0) candidate = value
    leader = -1

    for (k <- A.indices) {
      if (A(k) == candidate) count += 1
    }

    if (count > A.length / 2) leader = candidate

    leader
  }
}

class Lesson_6_Test extends FlatSpec {
  s"EquiLeader" should s"return expected value" in {
    assert(Solution.solution(Array(4, 3, 4, 4, 4, 2)) == 2, println(s"${Array(4, 3, 4, 4, 4, 2).deep} should return 2"))
  }
}
