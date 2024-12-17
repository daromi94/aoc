import scala.io.Source

type Grid      = Array[Array[Char]]
type Direction = (Int, Int)

def readGrid(lines: Iterator[String]): Grid =
  lines.map { _.toCharArray }.toArray

object Counter:
  private val Keyword: Array[Char] = Array('X', 'M', 'A', 'S')

  private val Directions: Array[Direction] =
    Array(
      (0, 1),  // ⬇️
      (0, -1), // ⬆️
      (1, 0),  // ➡️
      (-1, 0), // ⬅️
      (1, 1),  // ↘️
      (-1, 1), // ↙️
      (1, -1), // ↗️
      (-1, -1) // ↖️
    )

  def count(grid: Grid): Int =
    var occurrences = 0

    for
      i <- grid.indices
      j <- grid(0).indices if grid(i)(j) == Keyword.head
    do
      occurrences += countFromOrigin(grid, i, j)

    occurrences

  private def countFromOrigin(grid: Grid, i: Int, j: Int) =

    def matchesInDirection(direction: Direction): Boolean =
      val (dr, dc) = direction

      Keyword.indices.forall { k =>
        val (x, y) = (i + dr * k, j + dc * k)

        inBounds(grid, x, y) && grid(x)(y) == Keyword(k)
      }

    Directions.count { matchesInDirection }

  private def inBounds(grid: Grid, i: Int, j: Int): Boolean =
    0 <= i && i < grid.length && 0 <= j && j < grid(0).length

@main def main(args: String*): Unit =
  val lines = Source.stdin.getLines()

  val grid = readGrid(lines)

  val res = Counter.count(grid)

  println(s"Result: $res")
