package Lesson_2.MissingInteger

object Solution {
  def solution(A: Array[Int]): Int = {
    val t0 = System.nanoTime()
    println(A.deep)

    val check = Array.ofDim[Boolean](countPositive(A))
    val result = checkPerm(A, check)

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def countPositive(A:Array[Int], I:Int = 0, C:Int = 0): Int = {
    if (I >= A.length) return C + 1
    var count = C
    if (A(I) > 0) count = C +1
    countPositive(A, I+1, count)
  }

  def checkPerm(A: Array[Int], C:Array[Boolean], I:Int = 0): Int = {
    if (I >= A.length) return getFirstPositive(C)

    if (A(I) > 0 && A(I) < C.length && !C(A(I))) C(A(I)) = true

    checkPerm(A, C, I + 1)
  }

  def getFirstPositive(C:Array[Boolean], I:Int = 1): Int = {
    if (I >= C.length || !C(I)) return I

    getFirstPositive(C, I +1)
  }

}

object Test extends App {
  println(Solution.solution(Array(-2147483648, 2147483647)))
  println(Solution.solution(Array(1)))
  println(Solution.solution(Array(2,3,1,5)))
  println(Solution.solution(Array(-2,2,3,1,5,6,8)))
  println(Solution.solution(Array(1, 3, 6, 4, 1, 2)))
}
