import scala.io.Source
import scala.util.Using

object Day1 {
  def computeFuel(input: Int): Int = (math.floor(input / 3) - 2).toInt

  def computeTotalFuel(input: Int, acc: Int): Int = {
    val currFuel = computeFuel(input)
    if (currFuel <= 0) {
      acc
    } else {
      computeTotalFuel(currFuel, acc + currFuel)
    }
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day1/input.txt")) { bufferedSource =>
      val arr = bufferedSource.getLines().toList.map(_.toInt)

      val part1 = arr.map(computeFuel).sum

      println(s"Part 1: $part1")

      val part2 = arr.map(computeTotalFuel(_, 0)).sum

      println(s"Part 2: $part2")
    }
  }
}
