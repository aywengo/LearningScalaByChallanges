package Lesson_5.Brackets

import scala.collection.mutable

object Solution {
  def solution(S: String): Int = {
    val brackets,curly, square = mutable.Stack[Int]()
    val lastopen = mutable.Stack[Char]()

    for (c <- S) {
      if (c == ')') {
        if (brackets.isEmpty || lastopen.pop() != '(') return 0
        brackets.pop()
      }
      else if (c == '('){
        brackets.push(1)
        lastopen.push(c)
      }
      else if (c == ']') {
        if (square.isEmpty  || lastopen.pop() != '[') return 0
        square.pop()
      }
      else if (c == '['){
        square.push(1)
        lastopen.push(c)
      }
      else if (c == '}') {
        if (curly.isEmpty  || lastopen.pop() != '{') return 0
        curly.pop()
      }
      else if (c == '{'){
        curly.push(1)
        lastopen.push(c)
      }
    }
    if (lastopen.isEmpty && brackets.isEmpty && square.isEmpty && curly.isEmpty) return 1
    0
  }
}


object Test extends App {
  println(Solution.solution("{[()()]}")) // exp 1
  println(Solution.solution("([)()]")) // exp 0
  println(Solution.solution("[)()]")) // exp 0
}