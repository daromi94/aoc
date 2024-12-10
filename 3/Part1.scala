import scala.io.Source
import scala.util.matching.Regex

object Interpreter:

  private val Instruction: Regex = """mul\((\d+),(\d+)\)""".r

  def interpret(sections: Iterator[String]): Int =
    interpret(sections.mkString)

  private def interpret(section: String): Int =
    val instructions = Instruction.findAllIn(section)

    val products = instructions.collect {
      case Instruction(a, b) =>
        try
          a.toInt * b.toInt
        catch
          case e: NumberFormatException => throw new RuntimeException(s"$a * $b: invalid instruction", e)
    }

    products.sum

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines()

  val res = Interpreter.interpret(lines)

  println(s"Result: $res")
