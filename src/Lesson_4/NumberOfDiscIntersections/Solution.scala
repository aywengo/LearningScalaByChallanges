package Lesson_4.NumberOfDiscIntersections

object Solution {
  def solution(A: Array[Int]): Int = {
    val maxIterations = 10000000
    val downLimits, upLimits = Array.ofDim[Int](A.length)
    for (a <- A.indices) {
      if (A(a) >= A.length) {
        downLimits(0) += 1
        upLimits(A.length - 1) += 1
      } else {
        if (A(a) + a >= A.length) {
          upLimits(A.length-1) += 1
        } else {
          upLimits(a + A(a)) += 1
        }
        if (a - A(a) < 0) {
          downLimits(0) += 1
        } else {
          downLimits(a - A(a)) += 1
        }
      }
    }
    println(downLimits.deep)
    println(upLimits.deep)
    var result, inRange = 0

    for(i <-  downLimits.indices) {
      inRange += downLimits(i)

      if (inRange > 0 && upLimits(i) != 0) {
        result += sumIntersects(upLimits, inRange, i)
        if (result > maxIterations){
          return -1
        }
        inRange -= upLimits(i)
      }
    }

    result
  }

  def sumIntersects(UpLimits:Array[Int], InRange:Int, id:Int) : Int = {
    UpLimits(id) *  (2* InRange - 1 - UpLimits(id)) / 2
  }
}

object Test extends App {
  println(Solution.solution(Array(1, 5, 2, 1, 4, 0))) // exp 11
}