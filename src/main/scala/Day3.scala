import scala.io.Source
import scala.util.Using

object Day3 {
  case class Point(x: Int, y: Int)
  case class MovePoint(move: String, point: Point)

  def manhattanDist(p1: Point, p2: Point): Int = {
    math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
  }

  def intersects(i: Int,
                 j: Int,
                 firstPoints: Array[Point],
                 secondPoints: Array[Point]): Boolean = {
    firstPoints(i).x == secondPoints(j).x && firstPoints(i).y == secondPoints(j).y
  }

  def flattenPoints(movePoints: Array[MovePoint]): Seq[Point] = {
    val Array(firstMovePoint, secondMovePoint) = movePoints
    val MovePoint(_, firstPoint) = firstMovePoint
    val MovePoint(_, secondPoint) = secondMovePoint

    if (firstPoint.x == secondPoint.x) {
      val start = firstPoint.y
      val end = secondPoint.y
      val range =
        if (start < end) start + 1 to end else start - 1 to end by -1

      for {
        i <- range
      } yield {
        Point(firstPoint.x, i)
      }
    } else {
      val start = firstPoint.x
      val end = secondPoint.x
      val range =
        if (start < end) start + 1 to end else start - 1 to end by -1

      for {
        i <- range
      } yield {
        Point(i, firstPoint.y)
      }
    }
  }

  def addPoint(moveWithIdx: (String, Int)): MovePoint = {
    val (move, idx) = moveWithIdx

    if (idx == 0) {
      val point = move.head match {
        case 'R' => Point(move.tail.toInt, 0)
        case 'L' => Point(-move.tail.toInt, 0)
        case 'U' => Point(0, move.tail.toInt)
        case 'D' => Point(0, -move.tail.toInt)
      }

      MovePoint(move, point)
    } else {
      MovePoint(move, Point(0, 0))

    }
  }

  def getCoordinates(stringArr: Array[String]): Array[Point] = {
    // I don't like this
    var prevX = 0
    var prevY = 0
    stringArr.flatMap { move =>
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
  }

  def countMinSteps(first: Array[String], second: Array[String]): Int = {
    val firstMovePoints = first.zipWithIndex.map(addPoint)
    val secondMovePoints = second.zipWithIndex.map(addPoint)

    // first
    for {
      i <- 1 until first.length
    } {
      val j = i - 1
      val MovePoint(_, prevPoint) = firstMovePoints(j)
      val MovePoint(currMove, currPoint) = firstMovePoints(i)
      val newPoint = currMove.head match {
        case 'R' =>
          Point(prevPoint.x + currMove.tail.toInt, prevPoint.y + currPoint.y)
        case 'L' =>
          Point(prevPoint.x - currMove.tail.toInt, prevPoint.y + currPoint.y)
        case 'U' =>
          Point(prevPoint.x + currPoint.x, prevPoint.y + currMove.tail.toInt)
        case 'D' =>
          Point(prevPoint.x + currPoint.x, prevPoint.y - currMove.tail.toInt)
      }

      firstMovePoints(i) = MovePoint(currMove, newPoint)
    }

    // second
    for {
      i <- 1 until second.length
    } {
      val j = i - 1
      val MovePoint(_, prevPoint) = secondMovePoints(j)
      val MovePoint(currMove, currPoint) = secondMovePoints(i)
      val newPoint = currMove.head match {
        case 'R' =>
          Point(prevPoint.x + currMove.tail.toInt, prevPoint.y + currPoint.y)
        case 'L' =>
          Point(prevPoint.x - currMove.tail.toInt, prevPoint.y + currPoint.y)
        case 'U' =>
          Point(prevPoint.x + currPoint.x, prevPoint.y + currMove.tail.toInt)
        case 'D' =>
          Point(prevPoint.x + currPoint.x, prevPoint.y - currMove.tail.toInt)
      }

      secondMovePoints(i) = MovePoint(currMove, newPoint)
    }

    val firstPoints = (MovePoint("DUMMY", Point(0, 0)) +: firstMovePoints)
      .sliding(2)
      .flatMap(flattenPoints)
      .toArray
    val secondPoints = (MovePoint("DUMMY", Point(0, 0)) +: secondMovePoints)
      .sliding(2)
      .flatMap(flattenPoints)
      .toArray

    val intersections = for {
      i <- firstPoints.indices
      j <- secondPoints.indices
      if intersects(i, j, firstPoints, secondPoints)
    } yield {
      i + j + 2
    }

    intersections.min
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day3/input.txt")) { bufferedSource =>
      val input = bufferedSource.getLines().toArray
      val first = input(0).trim.split(",")
      val second = input(1).trim.split(",")

      val firstCoordinates = getCoordinates(first)
      val secondCoordinates = getCoordinates(second)

      val predicate = (p: Point) => p.x == 0 && p.y == 0 // Use .tail???
      val finalFirstCoordinates = firstCoordinates.filterNot(predicate).toSet
      val finalSecondCoordinates = secondCoordinates.filterNot(predicate).toSet
      val minDistPoint = finalFirstCoordinates
        .intersect(finalSecondCoordinates)
        .minBy(manhattanDist(_, Point(0, 0)))

      println(s"Part 1: ${manhattanDist(minDistPoint, Point(0, 0))}")

      val minSteps =
        countMinSteps(first, second)

      println(s"Part 2: $minSteps")
    }
  }
}
