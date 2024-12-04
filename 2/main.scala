import scala.io.Source

type Report  = Seq[Int]
type Reports = Seq[Report]

def readReports(lines: Iterator[String]): Reports =
  lines.zipWithIndex.map { parseLine }.toSeq

def parseLine(line: String, index: Int): Report =
  try
    line.split("\\s+").map { _.toInt }.toSeq
  catch
    case _: NumberFormatException => throw new IllegalArgumentException(s"invalid input at line ${index + 1}: '$line'")

def isSafe(report: Report): Boolean =
  if report.size < 2 then
    true
  else
    val adjacentDifferences = report.sliding(2).map { case Seq(a, b) => b - a }.toSeq

    // Levels must consistently increase or decrease
    val areLevelsMonotonic = adjacentDifferences.forall { _ > 0 } || adjacentDifferences.forall { _ < 0 }

    // Differences must fall within the range [1, 3]
    val areDifferencesInRange = adjacentDifferences.map { Math.abs }.forall { d => 1 <= d && d <= 3 }

    areLevelsMonotonic && areDifferencesInRange

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  val reports = readReports(lines)

  // Part 1
  val safeReports = reports.filter { isSafe }

  println(s"Safe Reports: ${safeReports.size}")
