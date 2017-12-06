val starting = io.Source.stdin.getLines().next().split('\t').map(_.toInt).toVector

def part1(rn: Vector[Int]): (Int, Vector[Vector[Int]]) = {
  var in: Vector[Int] = rn
  var visited: Vector[Vector[Int]] = Vector.empty
  var ind = -1

  while (ind < 0) {
    val (max, maxId) = in.zipWithIndex.reduceLeft({ (a, b) => if (a._1 >= b._1) a else b })
    visited = visited :+ in
    in = (1 to max).foldLeft(in.updated(maxId, 0)) { case (in, offset) =>
      val index = (offset + maxId) % in.length
      in.updated(index, in(index) + 1)
    }
    ind = visited.indexOf(in)
  }

  (ind, visited)
}

val (ind, res) = part1(starting)
println(res.size)
println(res.size - ind)