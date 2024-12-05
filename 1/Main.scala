import scala.io.Source

type Location     = Int
type Locations    = Seq[Location]
type LocationPair = (Location, Location)

def readLocations(lines: Iterator[String]): (Locations, Locations) =
  lines.zipWithIndex.map { parseLine }.toSeq.unzip

def parseLine(line: String, index: Int): LocationPair =
  line match
    case s"$l   $r" =>
      try
        (l.toInt, r.toInt)
      catch
        case _: NumberFormatException => throw new IllegalArgumentException(s"line ${index + 1}: invalid number")

    case _ => throw new IllegalArgumentException(s"line ${index + 1}: expected two elements")

def totalDistance(left: Locations, right: Locations): Int =
  val (leftSorted, rightSorted) = (left.sorted, right.sorted)

  val distances = leftSorted.lazyZip(rightSorted).map { (l, r) => math.abs(r - l) }

  distances.sum

def similarityScore(left: Locations, right: Locations): Int =
  val frequencies = right.groupBy { identity }.view.mapValues { _.size }

  val weights = left.map { l => l * frequencies.getOrElse(l, 0) }

  weights.sum

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines

  val (left, right) = readLocations(lines)

  // Part 1
  val td = totalDistance(left, right)

  println(s"Total Distance: $td")

  // Part 2
  val ss = similarityScore(left, right)

  println(s"Similarity Score: $ss")
