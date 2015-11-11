// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingJMBDMG-RBX/

package Lesson_5.Fish

import org.scalatest.FlatSpec

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

class Lesson_5_Test extends FlatSpec {
  def check(A: Array[Int], B: Array[Int], Expected: Int) = {
    s"Fish_${A.deep} and ${B.deep}" should s"return expected value $Expected " in {
      assert(Solution.solution(A, B) == Expected)
    }
  }

  check(Array(6, 3, 1, 2, 4, 5), Array(1, 1, 1, 0, 0, 0), 1)
  check(Array(6, 3, 1, 2, 4, 5, 7, 8), Array(1, 1, 1, 0, 0, 0, 1, 1), 3)
  check(Array(7, 6, 3, 1, 2, 4, 5), Array(0, 1, 1, 1, 0, 0, 0), 2)
  check(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 0, 0), 2)
  check(Array(1, 3, 4, 5, 2), Array(0, 1, 0, 0, 1), 4)
  check(Array(1, 3, 4, 5, 2, 6), Array(0, 0, 1, 1, 0, 1), 5)
  check(Array(2, 1, 5, 3, 4), Array(0, 1, 1, 1, 0), 3)
}
