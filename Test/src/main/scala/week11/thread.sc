import scala.util.Random

class HelloThread extends Thread {
  override def run(): Unit = {
    println("Hello World")
  }
}

val t = new HelloThread
t.start()
t.join()

var uidCount = 0L

def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread{
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }

  t.start()
  t.join()
}

startThread()
startThread()
startThread()
startThread()

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Double = {
  (for (i <- s until t) yield Math.pow(a(i), p)).sum
}

sumSegment(Array(1, 2, 3, 4, 5), 2, 1, 4)

def mcCount(iter: Int) = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0

  for (i <- 0 until iter) {
    val x = randomX.nextDouble
    val y = randomY.nextDouble

    if (x * x + y * y < 1) hits = hits + 1
  }

  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

monteCarloPiSeq(10000000)