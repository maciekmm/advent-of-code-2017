type Coord = (Int, Int)

class Direction(val coord: Coord, nextDir: => Direction) {
  def left = nextDir

  def right = left.left.left

  def +(loc: Coord): Coord = {
    (loc._1 + coord._1, loc._2 + coord._2)
  }
}

object Right extends Direction((1, 0), Up)

object Up extends Direction((0, -1), Left)

object Left extends Direction((-1, 0), Down)

object Down extends Direction((0, 1), Right)

abstract class State {
  def facing: Direction

  def position: Coord

  def grid: Map[Coord, Int]

  def tick(): (State, Int)

  def print(): Unit = {
    println("Facing: " + facing)
    for (i <- (-10 to 10)) {
      for (j <- (-10 to 10)) {
        val char = if (position == (j, i)) "0" else if (grid.contains((j, i))) "#" else "."
        printf("%s ", char)
      }
      println()
    }
    println()
    println()
  }
}

class State1(val facing: Direction, val position: Coord, val grid: Map[Coord, Int]) extends State {
  def tick(): (State1, Int) = {
    if (!grid.contains(position)) {
      (new State1(facing.left, facing.left + position, grid.updated(position, 1)), 1)
    } else {
      (new State1(facing.right, facing.right + position, grid.filter(_._1 != position)), 0)
    }
  }
}

class State2(val facing: Direction, val position: Coord, val grid: Map[Coord, Int]) extends State {
  def tick(): (State, Int) = {
    val current = grid.getOrElse(position, 0)
    val next = (current + 1) % 4

    val dir = current match {
      case 0 => facing.left //clean
      case 1 => facing //weakened
      case 2 => facing.right //infected
      case 3 => facing.right.right //flagged
    }

    if (next == 0) {
      (new State2(dir, dir + position, grid - position), 0)
    } else {
      (new State2(dir, dir + position, grid.updated(position, next)), if (next == 2) 1 else 0)
    }
  }
}


val raw = io.Source.stdin.getLines().toVector
val middle = raw.size / 2
val grid = raw.zipWithIndex.flatMap(l => l._1.zipWithIndex.filter(_._1 != '.').map(f => (f._2, l._2) -> 2)).toMap

def run(initial: State, iterations: Int): Int = {
  Iterator.iterate((initial, 0)) {
    state => {
      val (newState, lit) = state._1.tick()
      //newState.print
      (newState, state._2 + lit)
    }
  }.drop(iterations).take(1).next._2
}

println(s"Part 1: ${run(new State1(Up, (middle, middle), grid), 10000)}")
println(s"Part 2: ${run(new State2(Up, (middle, middle), grid), 10000000)}")
