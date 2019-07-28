package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      value <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(value, heap)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find minimum of 2 values") = forAll{ (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == (if (a > b) b else a)
  }

  property("insert and remove") = forAll{ (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("insert and remove of 2 values") = forAll { (a: Int, b: Int) =>
    findMin(deleteMin(insert(b, insert(a, empty)))) == (if (a > b)  a else b)
  }

  def constructHeap(l: List[Int]): H = l match {
    case Nil => empty
    case x :: xs => insert(x, constructHeap(xs))
  }

  property("insert value from list") = forAll { (l: List[Int]) =>
    l match {
      case _ :: _ :: _ => findMin(deleteMin(constructHeap(l))) == l.sorted.apply(1)
      case _ => true
    }
  }

  def toList(heap: H): List[Int] =
    if (isEmpty(heap))
      Nil
    else
      findMin(heap) :: toList(deleteMin(heap))

  def isSorted(l: List[Int]): Boolean = l match {
    case x :: y :: rest => x <= y && isSorted(rest)
    case _ => true
  }

  property("sort of heap") = forAll { (h: H) =>
    isSorted(toList(h))
  }

  property("minimum of two heaps") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2))
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
    else
      true
  }
}
