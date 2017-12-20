type Vec = (Int, Int, Int)

implicit class VecAdd(t: (Int, Int, Int)) {
  def +(p: (Int, Int, Int)) = (p._1 + t._1, p._2 + t._2, p._3 + t._3)
}

case class Particle(val index: Int, val position: Vec, val velocity: Vec, val acceleration: Vec) {
  def tick(): Particle = {
    val velocity = this.velocity + acceleration
    val position = this.position + velocity
    Particle(index, position, velocity, acceleration)
  }
}

val parse = "(-?[0-9]+)".r
val particles = io.Source.stdin.getLines().zipWithIndex.map({
  raw => {
    val n = parse.findAllIn(raw._1).map(_.toInt).toVector
    Particle(raw._2, (n(0), n(1), n(2)), (n(3), n(4), n(5)), (n(6), n(7), n(8)))
  }
}).toVector


val iterations = 5000;

val part1 = (1 to iterations).foldLeft(particles) {
  case (particles, _) => {
    particles.map(_.tick)
  }
}

println(s"Part 1: ${part1.minBy(a => math.abs(a.position._1) + math.abs(a.position._2) + math.abs(a.position._3)).index}")

val part2 = (1 to iterations).foldLeft(particles) {
  case (particles, _) => {
    particles.map(_.tick).groupBy(_.position).filter(_._2.size == 1).flatMap(_._2).toVector
  }
}
println(s"Part 2: ${part2.size}")

