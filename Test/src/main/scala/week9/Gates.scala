package week9

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(sig: Boolean): Unit =
      if (sig != sigVal) {
        sigVal = sig
        actions foreach (_())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSignal = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSignal}
    }

    input addAction invertAction
  }

  def andGate(a: Wire, b: Wire, c: Wire): Unit = {
    def andAction(): Unit = {
      val firstVal = a.getSignal
      val secondVal = b.getSignal
      afterDelay(AndGateDelay) { c setSignal (firstVal & secondVal) }
    }

    a addAction andAction
    b addAction andAction
  }

  def orGate(a: Wire, b: Wire, c: Wire): Unit = {
    def orAction(): Unit = {
      val firstVal = a.getSignal
      val secondVal = b.getSignal
      afterDelay(OrGateDelay) { c setSignal (firstVal | secondVal) }
    }

    a addAction orAction
    b addAction orAction

  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }

    wire addAction probeAction
  }
}
