// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/training58E4Z6-JGG/

package Lesson_5.Brackets

import org.scalatest.FlatSpec

import scala.collection.mutable

object Solution {
  def solution(S: String): Int = {
    val last = mutable.Stack[Char]()

    for (c <- S) c match {
      case '{' | '[' | '(' => last.push(c)
      case '}' if last.nonEmpty && last.pop() == '{' =>
      case ']' if last.nonEmpty && last.pop() == '[' =>
      case ')' if last.nonEmpty && last.pop() == '(' =>
      case _ => return 0
    }
    if (last.isEmpty) return 1
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
  check(")(", 0)
}