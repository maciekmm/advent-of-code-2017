val instructions = io.Source.stdin.getLines()
val startingState = instructions.next.split(' ').last.dropRight(1).last
val iterations = instructions.next.split(' ')(5).toInt
instructions.drop(1)

val states = instructions.grouped(10).map(st => {
  val state = st(0).split(' ').last.dropRight(1).last
  val subs = st.drop(1).grouped(4).take(2).map(sub => {
    val value = sub(1).split(' ').last.dropRight(1).toInt
    val direction = if (sub(2).trim.split(' ').last.dropRight(1) == "left") -1 else 1
    val next = sub(3).trim.split(' ').last.dropRight(1).last
    Substate(value, direction, next)
  }).toVector
  State(state, subs(0), subs(1))
}).map(a => (a.name -> a)).toMap

case class State(val name: Char, val zero: Substate, val one: Substate)

case class Substate(val value: Int, val movement: Int, val state: Char)

case class Machine(val tape: Map[Int, Int], val position: Int, val state: State)

val res = Iterator.iterate(Machine(Map().withDefaultValue(0), 0, states.get(startingState).get)) {
  machine => {
    val current = machine.tape.get(machine.position).getOrElse(0)
    val substate = if (current == 1) machine.state.one else machine.state.zero
    Machine(machine.tape.updated(machine.position, substate.value), machine.position + substate.movement, states.get(substate.state).get)
  }
}.drop(iterations).take(1).next
println(res.tape.count(_._2 == 1))