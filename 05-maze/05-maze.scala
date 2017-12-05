val grid = io.Source.stdin.getLines().map(_.toInt).toVector;

def solve(grid: Vector[Int], x: Int, c: Int, transform: Int => Int): Int = {
  grid.lift(x) match {
    case Some(off) =>
      solve(grid.updated(x, off + transform(off)), x + off, c + 1, transform)
    case _ => c
  }
}

println(solve(grid, 0, 0, _ => 1))
println(solve(grid, 0, 0, x => if (x >= 3) -1 else 1))
