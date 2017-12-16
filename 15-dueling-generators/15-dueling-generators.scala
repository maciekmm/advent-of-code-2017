val startingValues = io.Source.stdin.getLines().map(a => a.split(' ')(4).toInt).toSeq
val factors = Seq(16807, 48271)
val modulus = 2147483647

def generate(start: Long, factor: Int, divisibleBy: Int = 1): Iterator[Long] = {
  Iterator.iterate(start)(v => ((v * factor) % modulus)).withFilter(_ % divisibleBy == 0)
}

def filter(a: (Long, Long)): Boolean = ((a._1 ^ a._2) & 0xffff) == 0

println("Part 1: " + generate(startingValues(0), factors(0))
  .zip(generate(startingValues(1), factors(1))).take(40000000).count(filter))
println("Part 2: " + generate(startingValues(0), factors(0), 4)
  .zip(generate(startingValues(1), factors(1), 8)).take(5000000).count(filter))
