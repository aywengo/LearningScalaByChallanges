// The statement is copyrighted by Codility inc
//
// Materials for the lesson:
// https://codility.com/media/train/5-Stacks.pdf
//
// Results might be found under link:
// https://codility.com/demo/results/trainingH4AWBK-7Z5/

package Lesson_5.StoneWall

import org.scalatest.FlatSpec

import scala.collection.mutable

object Solution {
  def solution(H: Array[Int]): Int = {
    var amount = 0
    val stack  = mutable.Stack[Int]()
    println(H.deep)
    for (a <- H.indices) {
        cleanUpStack(stack, H(a))

      if (stack.isEmpty || stack.top != H(a)) {
        amount += 1
        stack.push(H(a))
        println(stack)
      }
    }
    amount
  }

  def cleanUpStack(S:mutable.Stack[Int], H:Int): Unit = {
    while (S.nonEmpty && S.top > H) S.pop()
  }
}

class Lesson_5_Test extends FlatSpec{
  s"StoneWall" should s"return expected value" in {
    assert(Solution.solution(Array(8,8,5,7,9,8,7,4,8)) == 7, println(s"${Array(8,8,5,7,9,8,7,4,8).deep} should return 7"))
  }
}