import scala.collection.immutable.Seq;

val ops = io.Source.stdin.getLines().next().split(',').map(a => (a.charAt(0), a.drop(1).split('/')))
val state = ('a' to 'p').toSeq

def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = seq.drop(seq.size - i) ++ seq.take(seq.size - i)

def transform[A](st: Seq[A], op: (Char, Array[String])): Seq[A] = {
  op._1 match {
    case 's' => rotateRight(st, op._2(0).toInt)
    case 'x' => {
      val spl = op._2.map(_.toInt)
      st.updated(spl(0), st(spl(1))).updated(spl(1), st(spl(0)))
    }
    case 'p' => {
      transform(st, ('x', Array(st.indexOf(op._2(0)(0)).toString, st.indexOf(op._2(1)(0)).toString)))
    }
  }
}

val it = Iterator.iterate(state)(
  a => ops.foldLeft(a) {
    case (st, op) => {
      transform(st, op)
    }
  }).zipWithIndex

println(s"Part 1: ${it.drop(1).next._1.mkString}")
val cycle = it.takeWhile(_._1 != ('a' to 'p')).size + 2
println(s"Part 2: ${it.take((10000000 - cycle) % cycle).drop((10000000 - cycle) % cycle - 1).next()._1.mkString}")


