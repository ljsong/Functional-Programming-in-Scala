import week9.{BankAccount, Circuits, Parameters, Simulation}

val acct = new BankAccount
acct deposit 50

(1 until 3) foreach (i => ("abc" foreach (j => println(i + " " + j))))

object sim extends Circuits with Parameters
import sim._

val a, b, sum, carry = new Wire

halfAdder(a, b, sum, carry)
probe("sum", sum)
probe("carry", carry)

a setSignal true
run()

b setSignal true
run()
