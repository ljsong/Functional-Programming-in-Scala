package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s1 = union(singletonSet(1), singletonSet(3))
  val s2 = union(s1, union(singletonSet(2), singletonSet(4)))
  printSet(s2)
  println(exists(s2, (x: Int) => x % 2 == 1))
}
