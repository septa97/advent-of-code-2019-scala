import scala.io.{Source, StdIn}
import scala.util.Using

object Day5 {
  def padOpcode(opcode: Int): String = {
    val opcodeStr = opcode.toString
    val pad = for (_ <- 0 until 4 - opcodeStr.length) yield "0"

    (pad ++ Array(opcodeStr)).mkString
  }

  def runProgram2(programArr: Array[Int]): Unit = {
    var curr = 0

    while (programArr(curr) != 99) {
      val opcode = padOpcode(programArr(curr))

      opcode.last match {
        // add or multiply
        case code: Char if code == '1' || code == '2' =>
          val firstOperandMode = opcode(1)
          val secondOperandMode = opcode(0)
          val addressToStore = programArr(curr + 3)
          val a = if (firstOperandMode == '0') {
            programArr(programArr(curr + 1))
          } else {
            programArr(curr + 1)
          }
          val b = if (secondOperandMode == '0') {
            programArr(programArr(curr + 2))
          } else {
            programArr(curr + 2)
          }

          val result = if (code == '1') a + b else a * b

          programArr(addressToStore) = result

          curr += 4
        // input
        case '3' =>
          val num = StdIn.readInt()
          val addressToStore = programArr(curr + 1)
          programArr(addressToStore) = num
          curr += 2
        // output
        case '4' =>
          val addressToRead = programArr(curr + 1)
          val num = programArr(addressToRead)
          println(num)
          curr += 2
        // jump if true or jump if false
        case code: Char if code == '5' || code == '6' =>
          val firstOperandMode = opcode(1)
          val secondOperandMode = opcode(0)
          val a = if (firstOperandMode == '0') {
            programArr(programArr(curr + 1))
          } else {
            programArr(curr + 1)
          }
          val b = if (secondOperandMode == '0') {
            programArr(programArr(curr + 2))
          } else {
            programArr(curr + 2)
          }

          if (code == '5') {
            if (a != 0) {
              curr = b
            } else {
              curr += 3
            }
          } else {
            if (a == 0) {
              curr = b
            } else {
              curr += 3
            }
          }
        // less than
        case code: Char if code == '7' || code == '8' =>
          val firstOperandMode = opcode(1)
          val secondOperandMode = opcode(0)
          val addressToStore = programArr(curr + 3)
          val a = if (firstOperandMode == '0') {
            programArr(programArr(curr + 1))
          } else {
            programArr(curr + 1)
          }
          val b = if (secondOperandMode == '0') {
            programArr(programArr(curr + 2))
          } else {
            programArr(curr + 2)
          }
          val result = if (code == '7') {
            if (a < b) 1 else 0
          } else {
            if (a == b) 1 else 0
          }

          programArr(addressToStore) = result

          curr += 4
      }
    }

    ()
  }

  def runProgram(programArr: Array[Int]): Unit = {
    var curr = 0

    while (programArr(curr) != 99) {
      val opcode = padOpcode(programArr(curr))

      opcode.last match {
        // add or multiply
        case code: Char if code == '1' || code == '2' =>
          val firstOperandMode = opcode(1)
          val secondOperandMode = opcode(0)
          val addressToStore = programArr(curr + 3)
          val a = if (firstOperandMode == '0') {
            programArr(programArr(curr + 1))
          } else {
            programArr(curr + 1)
          }
          val b = if (secondOperandMode == '0') {
            programArr(programArr(curr + 2))
          } else {
            programArr(curr + 2)
          }

          val result = if (code == '1') a + b else a * b

          programArr(addressToStore) = result

          curr += 4
        // input
        case '3' =>
          val num = StdIn.readInt()
          val addressToStore = programArr(curr + 1)
          programArr(addressToStore) = num
          curr += 2
        // output
        case '4' =>
          val addressToRead = programArr(curr + 1)
          val num = programArr(addressToRead)
          println(num)
          curr += 2
      }
    }

    ()
  }

  def main(args: Array[String]): Unit = {
    Using.resource(Source.fromFile("input/day5/input.txt")) { bufferedSource =>
      val program = bufferedSource.mkString.trim
      val originalProgramArr = program.split(",").map(_.toInt)

      val programArr = originalProgramArr.clone() // Not sure if clone() is needed but just to be sure
      val programArr2 = originalProgramArr.clone()
      runProgram(programArr) // part 1 (input 1)
      runProgram2(programArr2) // part 2 (input 5)
    }
  }
}
