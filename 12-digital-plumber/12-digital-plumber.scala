import scala.collection.mutable

var neighbors = mutable.Map[Int, mutable.Set[Int]]()

def addEdge(to: Int, edge: Int) = {
  if (neighbors.contains(to)) {
    neighbors(to).add(edge)
  } else {
    neighbors(to) = mutable.Set[Int](edge)
  }
}

io.Source.stdin.getLines().foreach(node => {
  val split = node.split("<->")
  val key = split(0).trim.toInt
  val connected = split(1).split(',').map(_.trim.toInt).toSet
  for (c <- connected) {
    addEdge(c, key)
    addEdge(key, c)
  }
});

def findGroup(group: Int): mutable.Set[Int] = {
  var pipes = mutable.Set[Int](group)
  var pipesToVisit = neighbors(group)

  while (pipesToVisit.nonEmpty) {
    var newToVisit = mutable.Set[Int]()
    for (pipe <- pipesToVisit) {
      pipes += pipe
      for (n <- neighbors(pipe)) {
        if (!pipes.contains(n)) {
          newToVisit += n
          pipes += n
        }
      }
    }
    pipesToVisit = newToVisit
  }
  return pipes
}

println(s"Part 1: ${findGroup(0).size}")

var groups = 0;
while (neighbors.nonEmpty) {
  neighbors --= findGroup(neighbors.head._1)
  groups += 1
}

println(s"Part 2: $groups")

