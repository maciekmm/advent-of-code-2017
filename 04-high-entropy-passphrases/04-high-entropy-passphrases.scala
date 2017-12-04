val grid = io.Source.stdin.getLines().toSeq;
println(s"Part 1: ${grid.map(_.split(' ')).filter(a => a.toSet.size == a.size).size}")
println(s"Part 2: ${grid.map(_.split(' ').map(_.sorted)).filter(a => a.toSet.size == a.size).size}")
