import scala.io.Source
import scala.util.Using

object Day3 {
  case class Point(x: Int, y: Int)

  def manhattanDist(p1: Point, p2: Point): Int = {
    math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day3/input.txt")) { bufferedSource =>
      val input = bufferedSource.getLines().toArray
      val first = input(0).trim.split(",")
      val second = input(1).trim.split(",")

      // I don't like this
      var prevX = 0
      var prevY = 0
      val firstCoordinates = first.flatMap { move =>
        val x = move.head match {
          case 'R' => move.tail.toInt
          case 'L' => -move.tail.toInt
          case _   => 0
        }

        val y = move.head match {
          case 'U' => move.tail.toInt
          case 'D' => -move.tail.toInt
          case _   => 0
        }

        val startX = math.min(prevX, prevX + x)
        val endX = math.max(prevX, prevX + x)
        val startY = math.min(prevY, prevY + y)
        val endY = math.max(prevY, prevY + y)
        val points = for {
          i <- startX to endX
          j <- startY to endY
        } yield Point(i, j)

        prevX += x
        prevY += y

        points
      }

      prevX = 0
      prevY = 0
      val secondCoordinates = second.flatMap { move =>
        val x = move.head match {
          case 'R' => move.tail.toInt
          case 'L' => -move.tail.toInt
          case _   => 0
        }

        val y = move.head match {
          case 'U' => move.tail.toInt
          case 'D' => -move.tail.toInt
          case _   => 0
        }

        val startX = math.min(prevX, prevX + x)
        val endX = math.max(prevX, prevX + x)
        val startY = math.min(prevY, prevY + y)
        val endY = math.max(prevY, prevY + y)
        val points = for {
          i <- startX to endX
          j <- startY to endY
        } yield Point(i, j)

        prevX += x
        prevY += y

        points
      }

      val predicate = (p: Point) => p.x == 0 && p.y == 0
      val finalFirstCoordinates = firstCoordinates.filterNot(predicate).toSet
      val finalSecondCoordinates = secondCoordinates.filterNot(predicate).toSet
      val minDistPoint = finalFirstCoordinates
        .intersect(finalSecondCoordinates)
        .minBy(manhattanDist(_, Point(0, 0)))

      println(s"Part 1: ${manhattanDist(minDistPoint, Point(0, 0))}")
    }
  }
}
