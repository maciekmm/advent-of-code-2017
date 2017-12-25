val components = io.Source.stdin.getLines().map(_.split('/').map(_.toInt)).map(a => Vector((a(0), a(1)), (a(1), a(0)))).flatten.toVector

def findBest(strength: Int, comps: Vector[(Int, Int)], socket: Int): Int = {
  val viable = comps.filter(_._1 == socket)
  if (viable.isEmpty) return strength
  viable.map(next => findBest(strength + next._1 + next._2, comps.diff(Vector(next, next.swap)), next._2)).max
}

def findLongest(strength: Int, comps: Vector[(Int, Int)], socket: Int): (Int, Int) = {
  val viable = comps.filter(_._1 == socket)
  if (viable.isEmpty) return (strength, components.diff(comps).length)
  val calc = viable.map(next => findLongest(strength + next._1 + next._2, comps.diff(Vector(next, next.swap)), next._2))
  calc.sortWith((a, b) => (a._2 > b._2 || (a._2 == b._2 && b._1 < a._1))).head
}

println(s"Part 1: ${findBest(0, components, 0)}")
println(s"Part 2: ${findLongest(0, components, 0)._1}")
