// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training3NZZXC-WRG/

package Lesson_5.Brackets

import org.scalatest.FlatSpec

import scala.collection.mutable

object Solution {
  def solution(S: String): Int = {
    val brackets, curly, square = mutable.Stack[Int]()
    val lastopen = mutable.Stack[Char]()

    S foreach {
      case ')' if brackets.nonEmpty && lastopen.pop() == '(' =>
        brackets.pop()
      case '(' =>
        brackets.push(1)
        lastopen.push('(')
      case ']' if square.nonEmpty && lastopen.pop() == '[' =>
        square.pop()
      case '[' =>
        square.push(1)
        lastopen.push('[')
      case '}' if curly.nonEmpty && lastopen.pop() == '{' =>
        curly.pop()
      case '{' =>
        curly.push(1)
        lastopen.push('{')
      case _ => return 0
    }
    if (lastopen.isEmpty && brackets.isEmpty && square.isEmpty && curly.isEmpty) return 1
    0
  }
}

class Lesson_5_Test extends FlatSpec {
  def check(Sut: String, Expected: Int) = {
    s"Brackets_$Sut" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"$Sut should return $Expected"))
    }
  }

  check("{[()()]}", 1)
  check("([)()]", 0)
  check("[)()]", 0)
}