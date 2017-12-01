var list = io.Source.stdin.filter((c) => c.isDigit).toList
println("Part 1:" + (list :+ list.head).sliding(2).toList.collect { case points if points.head == points.last => points.head.asDigit }.sum)
println("Part 2:" + list.indices.collect { case point if list(point) == list((point + (list.length / 2)) % list.length) => list(point).asDigit }.sum)