val f: PartialFunction[String, String] = { case "ping" => "pong"}
f("ping")
f.isDefinedAt("pong")

def isPrime(n:Int): Boolean = (2 to n / 2 + 1).forall(x => n % x != 0)

isPrime(16)
