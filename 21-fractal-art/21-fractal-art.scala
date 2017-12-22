implicit class Rotate[Boolean](t: List[List[Boolean]]) {
  def rotate(): List[List[Boolean]] = t.reverse.transpose

  def partition(n: Int): List[List[List[List[Boolean]]]] = {
    t.grouped(n).toList.map(_.map(_.grouped(n).toList).transpose)
  }
}

def parseSide(str: String): List[List[Boolean]] = {
  str.split('/').map(line => line.map(_ == '#').toList).toList
}

val patterns = io.Source.stdin.getLines().map(tr => {
  val spl = tr.split(" => ")
  val p = parseSide(spl(0))
  val pTo = parseSide(spl(1))

  Set(p, p.rotate, p.rotate.rotate, p.rotate.rotate.rotate, p.reverse, p.rotate.reverse, p.rotate.rotate.reverse, p.rotate.rotate.rotate.reverse).map(a => (a -> pTo))
}).toVector.flatten.toMap


val start = parseSide(".#./..#/###")


def count(it: Int): Int = {
  Iterator.iterate(start) {
    fractal => {
      val part = if (fractal.length % 2 == 0) 2 else 3
      fractal.partition(part).map(a => a.map(b => patterns.get(b).get)).flatMap(_.flatten.transpose)
    }
  }.drop(it).take(1).flatten.flatten.count(_ == true)
}

println(s"Part 1: ${count(5)}")

println(s"Part 2: ${count(18)}")



