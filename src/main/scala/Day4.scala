import scala.io.Source
import scala.util.Using

object Day4 {
  def isValidV2(str: String): Boolean = {
    val nonDecreasing = str.toSeq.sliding(2).map(_.unwrap).foldLeft(true) {
      (curr, c) =>
        val left = c(0).toInt
        val right = c(1).toInt
        val nonDecreasing = right >= left

        curr && nonDecreasing
    }

    val hasSameAdjacentDigits =
      str.zipWithIndex.toArray.sliding(2).foldLeft(false) { (curr, s) =>
        val Array((char1, idx1), (char2, idx2)) = s

        val notPartLeft =
          if (idx1 == 0) true
          else if (idx1 > 0 && str(idx1 - 1) == char1) false
          else true

        val notPartRight =
          if (idx2 == str.length - 1) true
          else if (idx2 < str.length - 1 && str(idx2 + 1) == char2) false
          else true

        val sameDigits = char1 == char2

        curr || (notPartLeft && sameDigits && notPartRight)
      }

    nonDecreasing && hasSameAdjacentDigits
  }

  def isValid(str: String): Boolean = {
    val (nonDecreasing, hasSameAdjacentDigits) =
      str.toSeq.sliding(2).map(_.unwrap).foldLeft((true, false)) { (curr, c) =>
        val (nonDecreasingAcc, hasSameAdjacentDigitsAcc) = curr
        val left = c(0).toInt
        val right = c(1).toInt

        val nonDecreasing = right >= left
        val sameDigits = left == right

        (nonDecreasingAcc && nonDecreasing) -> (hasSameAdjacentDigitsAcc || sameDigits)
      }

    nonDecreasing && hasSameAdjacentDigits
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day4/input.txt")) { bufferedSource =>
      val Array(lower, upper) =
        bufferedSource.mkString.trim.split("-").map(_.toInt)

      val result1 = for {
        num <- lower to upper
      } yield isValid(num.toString)

      val result2 = for {
        num <- lower to upper
      } yield isValidV2(num.toString)

      println(s"Part 1: ${result1.count(identity)}")
      println(s"Part 2: ${result2.count(identity)}")
    }
  }
}
