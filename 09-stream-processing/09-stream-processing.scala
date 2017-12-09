val input = io.Source.stdin.getLines().next()
val exclamation = "!.".r
val inside = "<[^>]*>".r

var leftover = exclamation.replaceAllIn(input, "")
val size = inside.findAllMatchIn(leftover).size
var noGarbage = inside.replaceAllIn(leftover, "")

println(s"Part 2: ${leftover.length - noGarbage.length - 2 * size}")

println(s"Part 1: ${
  noGarbage.foldLeft((0, 0)) { case ((score, depth), char) => {
    char match {
      case '{' => (score + depth + 1, depth + 1)
      case '}' => (score, depth - 1)
      case _ => (score, depth)
    }
  }
  }._1
}")

