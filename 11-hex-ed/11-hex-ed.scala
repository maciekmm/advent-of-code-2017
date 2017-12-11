val dirs = io.Source.stdin.getLines().next().split(',').toVector

val fin = dirs.foldLeft((0, 0, 0, 0)) {
  case ((x, y, z, max), dir) => {
    val next = dir match {
      case "n" => (x, y + 1, z - 1)
      case "nw" => (x - 1, y + 1, z)
      case "sw" => (x - 1, y, z + 1)
      case "s" => (x, y - 1, z + 1)
      case "se" => (x + 1, y - 1, z)
      case "ne" => (x + 1, y, z - 1)
      case a => throw new Exception(s"Invalid direction $a")
    }
    val dist = (math.abs(next._1) + math.abs(next._2) + math.abs(next._3)) / 2
    (next._1, next._2, next._3, if (dist > max) dist else max)
  }
}

println(s"Part 1: ${(math.abs(fin._1) + math.abs(fin._2) + math.abs(fin._3)) / 2}")
println(s"Part 2: ${fin._4}")
