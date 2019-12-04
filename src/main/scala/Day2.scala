import scala.io.Source
import scala.util.Using

object Day2 {
  case class Result(noun: Int, verb: Int, ans: Int)

  def bruteForce(originalProgramArr: Array[Int]): Result = {
    val goal = 19690720

    // Not the most optimal style 'cause I'm trying to make it FP-style
    val result = for {
      noun <- 0 to 99
      verb <- 0 to 99
      programArr = originalProgramArr.clone()
    } yield {
      programArr(1) = noun
      programArr(2) = verb

      Result(noun, verb, runProgram(programArr))
    }

    result.filter(result => result.ans == goal).head
  }

  def runProgram(programArr: Array[Int]): Int = {
    var curr = 0

    while (programArr(curr) != 99) {
      val a = programArr(programArr(curr + 1))
      val b = programArr(programArr(curr + 2))

      programArr(curr) match {
        case 1 =>
          programArr(programArr(curr + 3)) = a + b
        case 2 =>
          programArr(programArr(curr + 3)) = a * b
      }

      curr += 4
    }

    programArr(0)
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day2/input.txt")) { bufferedSource =>
      val program = bufferedSource.mkString.trim
      val originalProgramArr = program.split(",").map(_.toInt)

      val programArr = originalProgramArr.clone() // Not sure if clone() is needed but just to be sure
      programArr(1) = 12
      programArr(2) = 2
      println(s"Part 1: ${runProgram(programArr)}")

      val Result(noun, verb, _) = bruteForce(originalProgramArr)

      println(s"Part 2: ${100 * noun + verb}")
    }
  }
}
