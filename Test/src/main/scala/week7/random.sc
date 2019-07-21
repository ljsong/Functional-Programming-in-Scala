trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S]{
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S]{
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] = {
  for (x <- integers) yield lo + x % (hi - lo)
}

def oneOf[T](xs: T*): Generator[T] = {
  for (idx <- choose(0, xs.length)) yield xs(idx)
}

choose(3, 9).generate
oneOf((1, 2, 3)).generate

def lists: Generator[List[Int]] = {
  def emptyLists = single(Nil)
  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
}

trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def trees: Generator[Tree] = {
  def leafs = for (x <- integers) yield Leaf(x)
  def inners = for(left <- trees; right <- trees) yield Inner(left, right)
  for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree
}

trees.generate