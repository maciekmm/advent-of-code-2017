val grid = io.Source.stdin.getLines().map(_.split('\t').map(_.toInt).sorted).toSeq;

println(s"Part 1: ${grid.collect { case l => l.last - l.head }.sum}")
println(s"Part 2: ${grid.map(_.combinations(2).collectFirst { case a if a(1) % a(0) == 0 => a(1) / a(0) }.sum).sum}")
