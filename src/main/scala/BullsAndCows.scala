object BullsAndCows extends App {

  println(bullsAndCows("1807", "7810"))

  def buildMap(charList: List[Char]) = {
//    TODO avoid adding several times
    charList.foldLeft[Map[Char, Int]](Map())((agg, curr) => {
      agg + (curr -> charList.count(_ == curr))
    })
  }

  def iterateOverSecretToGetBullsAndCows(secret: List[Char], guess: Map[Char, Int]): Int = {
    secret.foldLeft[(Int, Map[Char, Int])](0, guess)((agg, curr) => {
      val cows = agg._1
      val currentGuess: Map[Char, Int] = agg._2

      if (currentGuess.contains(curr)) {
        (cows + 1,
          if (currentGuess(curr) > 1) {
            currentGuess + (curr -> (currentGuess(curr) - 1))
          } else {
            currentGuess - curr
          }
        )
      } else {
        (cows, currentGuess)
      }
    })._1
  }

  def bullsAndCows(secret: String, guess: String): String = {
    val bulls = (secret.toString zip guess.toList).foldLeft[(Int, List[Char], List[Char])](0, List(), List())((agg, curr) => {
      if(curr._1 == curr._2) {
        (agg._1 + 1, agg._2, agg._3)
      } else {
        (agg._1, agg._2 :+ curr._1.charValue(), agg._3 :+ curr._2.charValue())
      }
    })

    val cows = iterateOverSecretToGetBullsAndCows(bulls._2, buildMap(bulls._3))
    s"${bulls._1}A${cows}B"
  }

}
