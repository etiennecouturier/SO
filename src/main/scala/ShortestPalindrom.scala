object ShortestPalindrom extends App {

  def shortestPalindrome(input: String) = {
    helper(input, input.reverse, "")
  }

  def helper(str: String, reversedStr: String, removed: String): String = {
    if (str == reversedStr) {
      removed + str + removed
    } else {
      removed + helper(str.substring(0, str.length - 1), reversedStr.substring(1), reversedStr.substring(0, 1)) + removed
    }
  }

  println(shortestPalindrome("baaabc"))

}
