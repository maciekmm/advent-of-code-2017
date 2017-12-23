val instructions = io.Source.stdin.getLines().map(_.split(' ').last).toVector

val lower = instructions(0).toInt
println(s"Part 1: ${math.pow((lower - 2), 2).toInt}")

val lowerMult = lower * instructions(4).toInt - instructions(5).toInt
println(s"Part 2: ${
  (lowerMult to (lowerMult - instructions(7).toInt) by -instructions(instructions.length - 2).toInt).filter(a => {
    (2 to math.sqrt(a).toInt).filter(y => a % y == 0).size != 0
  }).size
}")
