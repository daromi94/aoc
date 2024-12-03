import scala.io.Source

type LocationPair = (Int, Int)
type Locations    = Seq[Int]

def readLocations(lines: Iterator[String]): (Locations, Locations) =
  val locationPairs = lines.zipWithIndex.map { parseLocationPair }.toSeq

  val left  = locationPairs.map(_._1)
  val right = locationPairs.map(_._2)

  (left, right)

def parseLocationPair(line: String, index: Int): LocationPair =
  val matches = line.split("\\s+")

  require(matches.length == 2, s"invalid format at line ${index + 1}, expected two elements")

  val l = matches(0).toInt
  val r = matches(1).toInt

  (l, r)

def calculateTotalDistance(left: Locations, right: Locations): Int =
  val (sortedLeft, sortedRight) = (left.sorted, right.sorted)

  val distances = sortedLeft.zip(sortedRight).map { case (l, r) => Math.abs(r - l) }

  distances.sum

def calculateSimilarityScore(left: Locations, right: Locations): Int =
  val rightCounts = right.groupMapReduce(identity)(_ => 1)(_ + _)

  val weights = left.map { l => l * rightCounts.getOrElse(l, 0) }

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
