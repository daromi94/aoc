import scala.io.Source
import scala.util.matching.Regex

object Interpreter:

  private val Multiply: Regex = """mul\((\d+),(\d+)\)""".r

  private val Do: Regex = """do\(\)""".r

  private val `Don't`: Regex = """don't\(\)""".r

  private val Instruction: Regex = s"$Multiply|$Do|${`Don't`}".r

  private case class State(accumulator: Int, enabled: Boolean)

  def interpret(sections: Iterator[String]): Int =
    interpret(sections.mkString)

  private def interpret(section: String): Int =
    val startState = State(accumulator = 0, enabled = true)

    val instructions = Instruction.findAllIn(section)

    val endState = instructions.foldLeft(startState) { case (state, instruction) => interpret(instruction, state) }

    endState.accumulator

  private def interpret(instruction: String, state: State): State =
    instruction match
      case Multiply(a, b) if state.enabled =>
        try
          val product = a.toInt * b.toInt

          state.copy(accumulator = state.accumulator + product)
        catch
          case e: NumberFormatException => throw new RuntimeException(s"$a * $b: invalid instruction", e)

      case Do() => state.copy(enabled = true)

      case `Don't`() => state.copy(enabled = false)

      case _ => state

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines()

  val res = Interpreter.interpret(lines)

  println(s"Result: $res")
