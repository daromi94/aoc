import scala.io.Source
import scala.util.matching.Regex

object Scanner:
  private val Instruction: Regex = """mul\((\d+),(\d+)\)""".r

  def scan(sections: Iterator[String]): Int =
    sections.map { scan }.sum

  def scan(section: String): Int =
    val instructions = Instruction.findAllIn(section)

    instructions.collect {
      case Instruction(a, b) =>
        try
          a.toInt * b.toInt
        catch
          case e: NumberFormatException => throw new RuntimeException(s"$a * $b: invalid instruction", e)
    }.sum

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  // Part 1
  val res = Scanner.scan(lines)

  println(s"Result: $res")
