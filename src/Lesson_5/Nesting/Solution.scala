package Lesson_5.Nesting

import scala.collection.mutable

object Solution {
  def solution(S: String): Int = {
    val stack = mutable.Stack[Int]()

    for (c <- S) {
      if (c == ')') {
        if (stack.isEmpty) return 0
        stack.pop()
      }
      else {
        stack.push(1)
      }
    }
    if (stack.isEmpty) return 1
    0
  }
}


object Test extends App {
  println(Solution.solution("(()(())())")) // exp 1
  println(Solution.solution("(()")) // exp 0
  println(Solution.solution("())")) // exp 0

}