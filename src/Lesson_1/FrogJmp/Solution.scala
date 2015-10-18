package Lesson_1.FrogJmp

object Solution {
  def solution(X: Int, Y: Int, D: Int): Int = {
    val t0 = System.nanoTime()
    val result = BigDecimal((Y - X) / D.toDouble).setScale(0, BigDecimal.RoundingMode.UP).toInt
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

}

object Test extends App {
  println(Solution.solution(1,5,2))
}
