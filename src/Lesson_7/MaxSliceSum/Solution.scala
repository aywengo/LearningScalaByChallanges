package Lesson_7.MaxSliceSum

object Solution {
  def solution(A: Array[Int]): Int = {
    if (A.length == 1) return A(0)
    golden_max_slice(A)
  }

  def golden_max_slice(A: Array[Int]): Int = {
    var maxEnding,maxSlice,minItem = 0
    val maxItem = A.max
    if (maxItem < 0) {
      minItem = A.min
      maxEnding = minItem
      maxSlice = minItem
    }
    for(a <- A) {
      maxEnding = minItem max maxEnding + a
      maxSlice = maxSlice max maxEnding
    }
    println(maxSlice max maxItem)
    maxSlice max maxItem
  }
}

object Test extends App {
  assert(Solution.solution(Array(-2, 1, 1)) == 2)
  assert(Solution.solution(Array(3, 2, -6, 4, 0)) == 5)
  assert(Solution.solution(Array(3, 2, -6, 3, 1)) == 5)
  assert(Solution.solution(Array(-10)) == -10)
  assert(Solution.solution(Array(-2, 1)) == 1)
  assert(Solution.solution(Array(-2, 3)) == 3)
  assert(Solution.solution(Array(-2, -2)) == -2)
}
