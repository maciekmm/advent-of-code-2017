val tubes = io.Source.stdin.getLines().zipWithIndex.flatMap(l => l._1.zipWithIndex.filter(_._1 != ' ').map(f => (l._2, f._2) -> f._1)).toMap
val starting = tubes.find(t => t._1._1 == 0 && t._2 == '|').get
type Coord = (Int, Int)

class Direction(val coord: Coord, nextDir: => Direction, prevDir: => Direction) {
  def next = nextDir

  def prev = prevDir

  def add(loc: Coord): Coord = {
    (loc._1 + coord._1, loc._2 + coord._2)
  }
}

object Right extends Direction((1, 0), Up, Down)

object Up extends Direction((0, 1), Left, Right)

object Left extends Direction((-1, 0), Down, Up)

object Down extends Direction((0, -1), Right, Left)

case class State(val pos: Coord, val lastDir: Direction, val visited: Seq[Char], val steps: Int)

val iterator = Iterator.iterate((State(starting._1, Down, Seq[Char](), 0), false)) {
  case (state, finished) => {
    var dir = state.lastDir
    var next = tubes.get(state.lastDir.add(state.pos))
    if (!next.isDefined) {
      next = tubes.get(state.lastDir.next.add(state.pos))
      dir = state.lastDir.next
    }
    if (!next.isDefined) {
      next = tubes.get(state.lastDir.prev.add(state.pos))
      dir = state.lastDir.prev
    }
    if (!next.isDefined) {
      (state, true)
    } else {
      if (next.get.isLetter) {
        (State(dir.add(state.pos), dir, state.visited :+ next.get, state.steps + 1), false)
      } else {
        (State(dir.add(state.pos), dir, state.visited, state.steps + 1), false)
      }
    }
  }
}
val result = iterator.takeWhile(!_._2).toVector.last._1
println(s"Part 1: ${result.visited.mkString}")
println(s"Part 2: ${result.steps + 1}")
