package Lesson_6.Dominator

object Solution {
  def solution(A: Array[Int]): Int = {
    A.indexOf(goldenLeader(A))
  }

  def goldenLeader(A: Array[Int]): Int = {
    var size,value, count, leader = 0
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

    if(count > A.length/2) leader = candidate

    leader
  }
}

object Test extends App {
  println(Solution.solution(Array(3, 4, 3, 2, 3, -1, 3, 3))) // exp 3
}
