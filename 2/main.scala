import scala.io.Source

type Report  = Seq[Int]
type Reports = Seq[Report]

def readReports(lines: Iterator[String]): Reports =
  lines.zipWithIndex.map { parseLine }.toSeq

def parseLine(line: String, index: Int): Report =
  try
    line.split("\\s+").map { _.toInt }.toSeq
  catch
    case _: NumberFormatException => throw new IllegalArgumentException(s"line ${index + 1}: invalid number")

def isSafe(report: Report): Boolean =
  if report.size < 2 then
    true
  else
    val steps = report.zip(report.tail).map { case (a, b) => b - a }

    // Levels must consistently increase or decrease
    val monotonic = steps.forall { _ > 0 } || steps.forall { _ < 0 }

    // Steps must be in [1, 3]
    val stepsInRange = steps.map { math.abs }.forall { s => 1 <= s && s <= 3 }

    monotonic && stepsInRange

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  val reports = readReports(lines)

  // Part 1
  val safeReports = reports.filter { isSafe }

  println(s"Safe Reports: ${safeReports.size}")
