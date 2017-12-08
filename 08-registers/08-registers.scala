
val input = io.Source.stdin.getLines()
val regex = "(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([>=!<]+) (-?\\d+)".r

type Registers = Map[String, Int]
var registers = Map[String, Int]().withDefaultValue(0)

class Condition(val register: String, val operator: String, val condition: Int) {
  def meets(registers: Registers): Boolean = {
    val reg = registers(register)
    operator match {
      case "!=" => reg != condition
      case "<=" => reg <= condition
      case ">=" => reg >= condition
      case "==" => reg == condition
      case ">" => reg > condition
      case "<" => reg < condition
      case _ => throw new Exception(s"Invalid operator ${operator}")
    }
  }
}

class Command(val register: String, val value: Int, val condition: Condition)

val proc = input.map(a => {
  val parts = regex.findFirstMatchIn(a).get;
  val modifier = if (parts.group(2) == "inc") 1 else -1;
  new Command(parts.group(1), modifier * parts.group(3).toInt, new Condition(parts.group(4), parts.group(5), parts.group(6).toInt))
}).foldLeft((registers, 0)) { case (in, command) =>
  if (command.condition.meets(in._1)) {
    val value = in._1(command.register) + command.value
    (in._1.updated(command.register, value), if (in._2 < value) value else in._2)
  } else {
    in
  }
}

println(s"Part 1: ${proc._1.maxBy(_._2)._2}")
println(s"Part 2: ${proc._2}")

