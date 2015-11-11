// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingT3X3RW-ETT/

package Lesson_5.Nesting

import org.scalatest.FlatSpec

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


class Lesson_5_Test extends FlatSpec {
  def check(Sut: String, Expected: Int) = {
    s"Nesting_$Sut" should s"return expected value $Expected " in {
      assert(Solution.solution(Sut) == Expected, println(s"$Sut should return $Expected"))
    }
  }

  check("(()(())())", 1)
  check("(()", 0)
  check("())", 0)
}