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

val part1 = knot(lengths.split(',').map(_.toInt), (0 to 255))

println(s"Part 1: ${part1(0) * part1(1)}")

val part2In = (lengths.map(_.toInt) ++ Vector(17, 31, 73, 47, 23))
val part2 = knot(List.fill(64)(part2In).flatMap(_.toVector), (0 to 255))
println(s"Part 2: ${
  part2.sliding(16, 16).map(_.foldLeft(0) { case (total, a) => total ^ a }).foldLeft("") { case (value, res) => {
    value + f"$res%02X"
  }
  }.toLowerCase
}")
