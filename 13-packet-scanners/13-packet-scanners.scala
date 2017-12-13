case class Layer(val depth: Int, val range: Int)

val layers = io.Source.stdin.getLines().map(a => {
  val spl = a.split(':')
  val key = spl(0).trim.toInt
  new Layer(key, spl(1).trim.toInt)
}).toVector

def severity(delay: Int): Int = {
  layers.foldLeft(0) { case (severity, layer) => {
    if ((delay + layer.depth) % (2 * layer.range - 2) == 0) {
      severity + layer.range * layer.depth
    } else severity
  }
  }
}

println(s"Part 1: ${severity(0)}")

var delay: Int = 10
//                             edge case if the first layer is the one that catches us
while (severity(delay) != 0 || (delay) % (2 * layers(0).range - 2) == 0) {
  delay += 1
}
println(s"Part 2: $delay")

