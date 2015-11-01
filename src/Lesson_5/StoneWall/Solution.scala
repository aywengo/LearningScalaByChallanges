package Lesson_5.StoneWall

import scala.collection.mutable

object Solution {
  def solution(H: Array[Int]): Int = {
    var amount = 0
    val stack  = mutable.Stack[Int]()
    println(H.deep)
    for (a <- H.indices) {
        cleanUpStack(stack, H(a))
        println(stack)

        if (stack.isEmpty || stack.top != H(a)) {
          amount += 1
          stack.push(H(a))
        }
    }
    amount
  }

  def cleanUpStack(S:mutable.Stack[Int], H:Int): Unit = {
    while (S.nonEmpty && S.top > H) S.pop()
  }
}


object Test extends App {
  println(Solution.solution(Array(8,8,5,7,9,8,7,4,8))) // exp 7
}