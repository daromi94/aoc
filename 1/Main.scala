import scala.io.Source

type Location     = Int
type Locations    = Seq[Location]
type LocationPair = (Location, Location)

def readLocations(lines: Iterator[String]): (Locations, Locations) =
  lines.map { parseLine }.toSeq.unzip

def parseLine(line: String): LocationPair =
  line.split("\\s+").toList match
    case l :: r :: Nil => (l.toInt, r.toInt)
    case _             => throw new IllegalArgumentException(s"invalid line format: '$line'")

def calculateTotalDistance(left: Locations, right: Locations): Int =
  val (leftSorted, rightSorted) = (left.sorted, right.sorted)

  val distances = leftSorted.zip(rightSorted).map { (l, r) => Math.abs(r - l) }

  distances.sum

def calculateSimilarityScore(left: Locations, right: Locations): Int =
  val frequencies = right.groupBy { identity }.view.mapValues { _.size }.toMap

  val weights = left.map { l => l * frequencies.getOrElse(l, 0) }

  weights.sum

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  val (left, right) = readLocations(lines)

  // Part 1
  val totalDistance = calculateTotalDistance(left, right)

  println(s"Total Distance: $totalDistance")

  // Part 2
  val similarityScore = calculateSimilarityScore(left, right)

  println(s"Similarity Score: $similarityScore")
