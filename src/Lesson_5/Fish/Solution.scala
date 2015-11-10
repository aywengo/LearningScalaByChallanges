package Lesson_5.Fish

import scala.collection.mutable

object Solution {
  def solution(A: Array[Int], B: Array[Int]): Int = {
    var sum = 0
    val down, up = mutable.Stack[Int]()
    for (i <- A.indices) {
      if (B(i) == 0) {
        if (up.isEmpty && down.isEmpty) sum += 1
        else {
          if (down.isEmpty) up.push(A(i))
          else {
            val t = stream(down, A(i))
            if (t == 0) sum += 1
            else {
              down.push(t)
            }
          }
        }
      }
      else {
        down.push(A(i))
      }
    }
    sum + down.length
  }

  def stream(Stack: mutable.Stack[Int], Fish: Int): Int = {
    while (Stack.nonEmpty) {
      val l = Stack.pop()
      if (Fish < l) {
        return l
      }
    }
    0
  }
}

object Test extends App {
  println(Solution.solution(Array(6, 3, 1, 2, 4, 5), Array(1, 1, 1, 0, 0, 0))) // exp 1
  println(Solution.solution(Array(6, 3, 1, 2, 4, 5, 7, 8), Array(1, 1, 1, 0, 0, 0, 1, 1))) // exp 3
  println(Solution.solution(Array(7, 6, 3, 1, 2, 4, 5), Array(0, 1, 1, 1, 0, 0, 0))) // exp 2
  println(Solution.solution(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 0, 0))) // exp 2
  println(Solution.solution(Array(1, 3, 4, 5, 2), Array(0, 1, 0, 0, 1))) // exp 4
  println(Solution.solution(Array(1, 3, 4, 5, 2, 6), Array(0, 0, 1, 1, 0, 1))) // exp 5
  println(Solution.solution(Array(2, 1, 5, 3, 4), Array(0, 1, 1, 1, 0))) // exp 3
}
