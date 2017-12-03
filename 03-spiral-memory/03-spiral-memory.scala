import scala.annotation.tailrec

def part1(input: Int): Int = {
  var x = math.sqrt(input).ceil
  if (x % 2 == 0) x += 1
  val half = (x - 1) / 2
  (math.abs(half - ((math.pow(x, 2) - input) % (x - 1))) + half).toInt
}

type Grid = Map[(Int, Int), Int]
type Coord = (Int, Int)

class Direction(val coord: Coord, val start: Int, nextDir: => Direction) {
  def next = nextDir

  def add(loc: Coord): Coord = {
    (loc._1 + coord._1, loc._2 + coord._2)
  }
}

object Right extends Direction((1, 0), -1, Up)

object Up extends Direction((0, 1), 1, Left)

object Left extends Direction((-1, 0), 0, Down)

object Down extends Direction((0, -1), 0, Right)

def sumAdjacent(input: Grid, coord: Coord): Int = {
  List((coord._1 - 1, coord._2 + 1), (coord._1, coord._2 + 1), (coord._1 + 1, coord._2 + 1),
    (coord._1 - 1, coord._2), (coord._1 + 1, coord._2),
    (coord._1 - 1, coord._2 - 1), (coord._1, coord._2 - 1), (coord._1 + 1, coord._2 - 1)).map(input).sum
}

@tailrec
def calcSeq(input: Int, grid: Grid, dir: Direction, coord: Coord, depth: Int, no: Int): Int = {
  val value = sumAdjacent(grid, coord)
  if (value >= input) {
    value
  } else {
    val newGrid = grid + (coord -> value)
    if (no >= depth) {
      if(dir.next == Up)
        calcSeq(input, newGrid, dir.next, dir.next.add(coord), depth+2, dir.next.start)
      else
        calcSeq(input, newGrid, dir.next, dir.next.add(coord), depth, dir.next.start)
    } else {
      calcSeq(input, newGrid, dir, dir.add(coord), depth, no + 1)
    }
  }
}

def part2(input: Int): Int = {
  calcSeq(input, Map(
    (0, 0) -> 1,
    (1, 0) -> 1
  ).withDefaultValue(0), Up, (1, 1), 1, 1)
}

val input = io.StdIn.readInt()
println(s"Part 1: ${part1(input)}")
println(s"Part 2: ${part2(input)}")
