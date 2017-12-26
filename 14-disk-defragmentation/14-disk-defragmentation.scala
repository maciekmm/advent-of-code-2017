import scala.collection.mutable

val lengths = io.Source.stdin.getLines().next()

case class State[A](val list: Seq[A], val skip: Int, val skipped: Int)

def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = seq.drop(i % seq.size) ++ seq.take(i % seq.size)

def knot(seq: Seq[Int], st: Seq[Int]): Seq[Int] = {
  val s = seq.foldLeft(State(st, 0, 0)) { case (state, length) => {
    val reversed = state.list.take(length).reverse
    State(rotateLeft(reversed ++ state.list.drop(length), state.skip + length), state.skip + 1, state.skipped + length + state.skip)
  }
  }
  rotateLeft(s.list, s.list.size - (s.skipped % s.list.size))
}

def knotHash(str: String, row: Int): String = {
  val part2In = (str + '-' + row).map(_.toInt) ++ Vector(17, 31, 73, 47, 23)
  val part2 = knot(List.fill(64)(part2In).flatMap(_.toVector), (0 to 255))
  scala.math.BigInt(part2.sliding(16, 16).map(_.foldLeft(0) { case (total, a) => total ^ a }).foldLeft("") { case (value, res) => {
    value + f"$res%02X"
  }
  }.toLowerCase, 16).toString(2)
}

val grid = (0 to 127).map(a => knotHash(input, a).toSeq)
val segments = grid.zipWithIndex.flatMap(a => a._1.zipWithIndex.map(b => (a._2, b._2) -> b._1)).toMap

def neighbors(coord: (Int, Int)): Vector[(Int, Int)] = {
  List((coord._1, coord._2 + 1),
    (coord._1 - 1, coord._2), (coord._1 + 1, coord._2),
    (coord._1, coord._2 - 1)).map(a => a -> segments.getOrElse(a, '0')).filter(_._2 == '1').map(_._1).toVector
}


println(s"Part 1: ${grid.map(a => a.count(_ == '1')).sum}")


val visited = mutable.Set[(Int, Int)]()

def traverseGroup(coord: (Int, Int)): Boolean = {
  if (visited.contains(coord) || segments.getOrElse(coord, '0') == '0') {
    return false
  }
  val queue = mutable.Queue.empty[(Int, Int)]
  queue += coord

  while (!queue.isEmpty) {
    val v = queue.dequeue
    visited.add(v)
    queue ++= neighbors(v).filterNot(a => visited.contains(a))
  }
  return true
}


println(s"Part 2: ${segments.map(a => traverseGroup(a._1)).count(_ == true)}")
