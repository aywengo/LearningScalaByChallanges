// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/6-Leader.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingB5J2KF-M86/


package Lesson_6.Dominator

import org.scalatest.FlatSpec

object Solution {
  def solution(A: Array[Int]): Int = {
    A.indexOf(goldenLeader(A))
  }

  def goldenLeader(A: Array[Int]): Int = {
    var size, value, leader = 0
    A.foreach(a => {
      if (size == 0) {
        size += 1
        value = a
      }
      else {
        if (value != a) size -= 1 else size += 1
      }
    })

    var candidate = -1
    if (size > 0) candidate = value
    leader = -1

    val count = A.count(a => a == candidate)

    if (count > A.length / 2) leader = candidate

    leader
  }
}

class Lesson_6_Test extends FlatSpec {
  s"Dominator" should s"return expected value" in {
    assert(Solution.solution(Array(3, 4, 3, 2, 3, -1, 3, 3)) == 0, println(s"${Array(3, 4, 3, 2, 3, -1, 3, 3).deep} should return 0"))
  }
}
