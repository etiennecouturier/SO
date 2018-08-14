

object BadVersion extends App {

  val versions: List[Int] = Range.Int(0, 10, 1).toList

  println(firstBadVersion(0, versions.size-1))

  def firstBadVersion(start: Int, end: Int): Int = {
    val middle = (end + start) / 2

    if (isBadVersion(versions(middle))) {
      if(!isBadVersion(versions(middle-1))) {
        middle
      } else {
        firstBadVersion(start, middle)
      }
    } else {
      if(isBadVersion(versions(middle+1))) {
        middle + 1
      } else {
        firstBadVersion(middle, end)
      }
    }
  }

  def isBadVersion(version: Int) = {
    if (version >= 10) {
      true
    } else {
      false
    }
  }
}