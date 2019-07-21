def from(n: Int): LazyList[Int] = n #:: from(n + 1)

val nats = from(0)

def sieve(s: LazyList[Int]): LazyList[Int] =
s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))
primes.take(100).toList