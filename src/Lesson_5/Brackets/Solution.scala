// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingT9U6EU-TFW/

package Lesson_5.Brackets

import org.scalatest.FlatSpec

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