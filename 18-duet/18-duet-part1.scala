import scala.collection.immutable.Map;

val instructions = io.Source.stdin.getLines().map(a => {
  val values = a.split(' ')
  Instruction(values(0), values(1), values.drop(2).headOption)
}).toVector

class Machine(val id: Long, val sn: Long => Unit) {
  var snd: Long => Unit = sn
  private var registers = Map[Char, Long]().updated('p', id).withDefaultValue[Long](0)
  private var currentOffset: Int = 0
  private var queue = Vector[Long]()
  private var waiting = false

  def run(): Unit = {
    if (waiting) {
      waiting = false
    }
    while (!waiting && currentOffset >= 0 && currentOffset < instructions.length) {
      var instruction = instructions(currentOffset)
      if (instruction.operation == "rcv") {
        if (queue.nonEmpty) {
          instruction = Instruction("set", instruction.register, Option(queue.head.toString))
          queue = queue.tail
        } else {
          waiting = true
        }
      }
      if (!waiting) {
        val res = instruction.execute(registers)
        if (instruction.operation == "snd") {
          snd(res._3.get)
        }
        currentOffset = currentOffset + res._2
        registers = res._1
      }
    }
  }

  def receive(v: Long): Unit = {
    queue = queue :+ v
    if (waiting) {
      run()
    }
  }
}

case class Instruction(val operation: String, val register: String, val argument: Option[String]) {
  def parseValue(registers: Map[Char, Long], argument: String): Long = {
    if (argument.size == 1 && argument(0).isLetter) {
      registers(argument(0))
    } else {
      argument.toLong
    }
  }

  // returns offset of next instruction to proceed
  def execute(registers: Map[Char, Long]): (Map[Char, Long], Int, Option[Long]) = {
    val value = parseValue(registers, register)
    val argumentValue = if (argument.isDefined) parseValue(registers, argument.get) else 0
    operation match {
      case "set" => (registers.updated(register(0), argumentValue), 1, None)
      case "add" => (registers.updated(register(0), value + argumentValue), 1, None)
      case "mul" => (registers.updated(register(0), value * argumentValue), 1, None)
      case "mod" => (registers.updated(register(0), value % argumentValue), 1, None)
      case "jgz" => (registers, if (value > 0) argumentValue.toInt else 1, None)
      case "snd" => {
        (registers, 1, Some(value))
      }
      case "rcv" => (registers, 1, if (value != 0) Some(value) else None)
      case _ => (registers, 1, None)
    }
  }
}

var last: Long = 0
new Machine(0, (a => {
  last = a
})).run()

println(s"Part 1: $last")

var counter = 0;
var machineA = new Machine(0, _ => ())
var machineB = new Machine(1, a => {
  counter += 1
  machineA.receive(a)
})
machineA.snd = a => machineB.receive(a)

machineA.run()
machineB.run()

println(s"Part 2: $counter")
