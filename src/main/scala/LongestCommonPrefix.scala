object LongestCommonPrefix extends App {

  def getLongestCommonPrefix(strings: List[String]) = {
    val initialMaxPrefix = getMaxPrefix(strings.head, strings(1))

    strings.foldLeft(initialMaxPrefix){(currentMaxPrefix, str) =>
        val curr = getMaxPrefix(str, currentMaxPrefix)
        if (curr.length < currentMaxPrefix.length) {
          curr
        } else {
          currentMaxPrefix
        }
    }

  }

  def getMaxPrefix(str1: String, str2: String): String = {
    str1
      .zip(str2)
      .takeWhile(Function.tupled(_ == _))
      .map(_._1)
      .mkString
  }

  val input = List("abcde", "abcd", "abc", "aabcd")
  println(getLongestCommonPrefix(input))

}
