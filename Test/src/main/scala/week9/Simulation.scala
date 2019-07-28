package week9

abstract class Simulation {
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]

  private var agenda: Agenda = List()

  private var curTime = 0

  def currentTime = curTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(curTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: Agenda, item: Event): Agenda = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ => item :: ag
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case _ =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulated started, time = " + currentTime + " ***")
    }
    loop()
  }
}
