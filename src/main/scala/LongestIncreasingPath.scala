object LongestIncreasingPath extends App {

  val arr: Array[Array[(Int, Boolean)]] = Array.ofDim[(Int, Boolean)](4, 4)
  arr(0)(0) = (5, false)
  arr(0)(1) = (2, false)
  arr(0)(2) = (4, false)
  arr(0)(3) = (6, false)
  arr(1)(0) = (1, false)
  arr(1)(1) = (3, false)
  arr(1)(2) = (0, false)
  arr(1)(3) = (2, false)
  arr(2)(0) = (4, false)
  arr(2)(1) = (5, false)
  arr(2)(2) = (1, false)
  arr(2)(3) = (9, false)
  arr(3)(0) = (9, false)
  arr(3)(1) = (8, false)
  arr(3)(2) = (3, false)
  arr(3)(3) = (7, false)

//  def longestIncreasingPath(): Int = {
//
//  }

  dfsOnMatrix(1, 1)

//  print2DArray(arr)

  private def print2DArray(arr: Array[Array[(Int, Boolean)]]): Unit = {
    for (a <- arr) {
      for (b <- a) {
        print(s"${b._1} ")
      }
      println("")
    }
  }

  def dfsOnMatrix(i: Int, j: Int): Unit = {
    arr(i)(j) = (arr(i)(j)._1, true)

    println(arr(i)(j)._1)

    if ((j-1) >= 0 && !arr(i)(j-1)._2 && arr(i)(j)._1 < arr(i)(j-1)._1) {
      dfsOnMatrix(i, j-1)
    }

    if ((i-1) >= 0 && !arr(i-1)(j)._2 && arr(i)(j)._1 < arr(i-1)(j)._1) {
      dfsOnMatrix(i-1, j)
    }

    if ((j+1) < arr.length && !arr(i)(j+1)._2 && arr(i)(j)._1 < arr(i)(j+1)._1) {
      dfsOnMatrix(i, j+1)
    }

    if ((i+1) < arr.length && !arr(i+1)(j)._2 && arr(i)(j)._1 < arr(i+1)(j)._1) {
      dfsOnMatrix(i+1, j)
    }
  }

}