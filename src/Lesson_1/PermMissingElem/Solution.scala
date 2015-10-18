package Lesson_1.PermMissingElem


object Solution {
  def solution(A: Array[Int]): Int = {
    val length = A.length
    val fact = ((length.toLong + 2) * (length + 1))/2

    val t0 = System.nanoTime()
    val result = (fact - A.sum).toInt
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

object Test extends App {
  println(Solution.solution(Array(2,3,1,5)))
}
