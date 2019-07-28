package week9

abstract class Circuits extends Gates{
  def halfAdder(a: Wire, b: Wire, sum: Wire, carry: Wire): Unit = {
    val d = new Wire
    val e = new Wire

    andGate(a, b, carry)
    orGate(a, b, d)
    inverter(carry, e)
    andGate(d, e, sum)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, cout: Wire, sum: Wire): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire

    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}
