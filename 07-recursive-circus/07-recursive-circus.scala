val starting = io.Source.stdin.getLines();

object Tower {
  val cityRegex = "(\\w+) \\((\\d+)\\)( -> (.*))?".r;

  def parse(disc: Iterator[String]): Disc = {
    val discs = starting.map(Tower.parse(_)).toVector
    discs.foreach(out => {
      out.above = out.above.map(a => {
        val child = discs.find(_.name == a.name).get
        child.parent = out;
        child
      })
    })

    var parent = discs(0)
    while (parent.parent != null) {
      parent = parent.parent
    }
    parent
  }

  def parse(disc: String): Disc = {
    val parts = cityRegex.findFirstMatchIn(disc).get;
    parts.group(4) match {
      case str: String => new Disc(parts.group(1), parts.group(2).toInt, str.split(',').map(a => new Disc(a.trim(), 0, Vector.empty)).toVector);
      case _ => new Disc(parts.group(1), parts.group(2).toInt, Vector.empty)
    }
  }

  class Disc(val name: String, val weight: Int, val ab: Vector[Disc]) {
    var parent: Disc = null;
    var above: Vector[Disc] = ab;

    def realWeight(): Int = {
      return weight + above.map(_.realWeight()).sum;
    }

    def fix(target: Int): Int = {
      if (above.isEmpty) {
        return if (weight == target) 0 else target
      }

      val realAbove = above.map(_.realWeight())
      val isBalanced = realAbove.distinct.size == 1
      val realSum = realAbove.sum

      if (isBalanced) {
        return if (target - realAbove.sum == weight) 0 else target - realSum
      }

      val weights = above.map(z => (z, z.realWeight())).sortBy(_._2)

      if (weights.head != weights(1)) {
        val a = weights.head._1.fix(weights(1)._2)
        if (a != 0) {
          return a
        }
      }

      if (weights.last._2 != weights.head._2) {
        return weights.last._1.fix(weights.head._2)
      }
      return 0
    }

    def findIncorrect(): Int = {
      fix(realWeight())
    }
  }

}

val parent = Tower.parse(starting)
println(s"Part 1: ${parent.name}")
println(s"Part 2: ${parent.findIncorrect()}")
