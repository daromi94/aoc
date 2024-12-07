import scala.io.Source
import scala.util.matching.Regex

object Scanner:
  private val InstructionPattern: Regex = "mul\\((?<n1>\\d+),(?<n2>\\d+)\\)".r

  def scan(sections: Iterator[String]): Int =
    sections.map { scan }.sum

  def scan(section: String): Int =
    InstructionPattern.findAllMatchIn(section).map { evaluate }.sum

  private def evaluate(instruction: Regex.Match): Int =
    try
      val n1 = instruction.group("n1").toInt
      val n2 = instruction.group("n2").toInt
      n1 * n2
    catch
      case e: NumberFormatException => throw new RuntimeException(s"${instruction.matched}: invalid instruction", e)

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  // Part 1
  val res = Scanner.scan(lines)

  println(s"Result: $res")
