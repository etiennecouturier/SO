object MinimumSumPathMatrix extends App {

  def findMinimumSumPath(A: Array[Array[Int]]): Array[Array[Int]] = {
    print2DArray(A)

    val M: Array[Array[Int]] = Array.ofDim[Int](4, 4)

    for ((_, i) <- A.view.zipWithIndex) {
      if (i == 0) {
        M(i)(0) = A(i)(0)
      } else {
        M(i)(0) = M(i-1)(0) + A(i)(0)
      }
    }

    for ((_, j) <- A.view.zipWithIndex) {
      if (j == 0) {
        M(0)(j) = A(0)(j)
      } else {
        M(0)(j) = M(0)(j-1) + A(0)(j)
      }
    }

    for ((a, i) <- A.view.zipWithIndex) {
      for ((_, j) <- a.view.zipWithIndex) {
        if (i != 0 && j != 0) {
          M(i)(j) = A(i)(j) + (Math min(M(i-1)(j), M(i)(j-1)))
        }
      }
    }

    M
  }

  private def print2DArray(arr: Array[Array[Int]]): Unit = {
    for (a <- arr) {
      for (b <- a) {
        print(s"$b ")
      }
      println("")
    }
  }

  val arr: Array[Array[Int]] = Array.ofDim[Int](4, 4)
  arr(0)(0) = 5
  arr(0)(1) = 2
  arr(0)(2) = 4
  arr(0)(3) = 6
  arr(1)(0) = 1
  arr(1)(1) = 3
  arr(1)(2) = 0
  arr(1)(3) = 2
  arr(2)(0) = 4
  arr(2)(1) = 5
  arr(2)(2) = 1
  arr(2)(3) = 9
  arr(3)(0) = 9
  arr(3)(1) = 8
  arr(3)(2) = 3
  arr(3)(3) = 7

  val res = findMinimumSumPath(arr)

  println()
  println("-----------------------------")
  println()

  print2DArray(res)

}