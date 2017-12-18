//val gap = io.Source.stdin.getLines.next.map(_.toInt)
val gap = 376

def insert[A](list: List[A], i: Int, values: A*) = {
  val (front, back) = list.splitAt(i)
  front ++ values ++ back
}

def getNumber(list: List[Int], currentPos: Int, after: Int, steps: Int): Int = {
  val l = (1 to steps).foldLeft((list, 0)) {
    case ((list, current), op) => {
      (insert(list, (current + gap) % list.size + 1, op), (current + gap) % list.size + 1)
    }
  }
  l._1(l._1.indexWhere(_ == after) + 1)
}

def part2(steps: Int): Int = {
  (1 to steps).foldLeft((0, 0)) {
    case ((current, last), step) => {
      val next = (current + gap) % step + 1
      (next, if (next == 1) step else last)
    }
  }._2
}

println(s"Part 1: ${getNumber(List(0), 0, 2017, 2017)}")
println(s"Part 2: ${part2(50000000)}")
