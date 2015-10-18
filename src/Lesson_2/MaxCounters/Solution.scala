package Lesson_2.MaxCounters

object Solution {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    println(A.deep)
    incByIndex(A, Array.ofDim[Int](N), N)
  }

  def incByIndex(A:Array[Int], R:Array[Int], N:Int, I:Int = 0, M:Int = 0, B:Int = 0): Array[Int] = {
    if(I >= A.length) {
      for(a <- R.indices) {
          if (R(a) < B)
            R(a) = B
        }
      return R
    }

    if (A(I) >= N + 1) {
      incByIndex(A, R, N, I+1, M, M)
    }
    else {
      if (R(A(I) - 1) < B) {
        R(A(I) - 1) = B + 1
      }
      else {
        R(A(I) - 1) += 1
      }

      val r = R(A(I) - 1)
      var max = M
      if (r > M) max = r
      incByIndex(A, R, N, I+1, max, B)
    }
  }
}

object Test extends App {
  println(Solution.solution(1, Array(1)).deep)
  println(Solution.solution(4, Array(2,3,2,1,5,1)).deep)
}