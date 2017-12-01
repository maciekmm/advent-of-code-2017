val l = io.Source.stdin.toSeq

def c(l: Seq[Char], s: Int): Int = {
  l.indices.filter(a => l(a) == l((a + s) % l.length)).map(l(_).asDigit).sum
}

println(s"Part 1: ${c(l, 1)}")
println(s"Part 2: ${c(l, l.length / 2)}")

