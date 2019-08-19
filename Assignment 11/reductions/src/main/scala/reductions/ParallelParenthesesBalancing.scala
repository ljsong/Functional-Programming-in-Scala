package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var ix, balanced = 0
    while (ix < chars.length && balanced >= 0) {
      if (chars(ix) == ')') balanced = balanced - 1
      else if (chars(ix) == '(') balanced = balanced + 1

      ix = ix + 1
    }
    ix >= chars.length && balanced == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, par_balanced: Int, remaining: Int): (Int, Int) = {
      // arg1 means whether it is balanced in this seq,
      // and arg2 means how many parentheses remained after balancing
      if (idx == until) return (par_balanced, remaining)
      if (chars(idx) == '(')
        if (par_balanced >= 0) traverse(idx + 1, until, par_balanced + 1, remaining + 1)
        else traverse(idx + 1, until, par_balanced, remaining + 1)
      else if (chars(idx) == ')') traverse(idx + 1, until, par_balanced - 1, remaining - 1)
      else traverse(idx + 1, until, par_balanced, remaining)
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      val par_balanced, remaining = 0
      if (until - from <= threshold) {
        val ret = traverse(from, until, par_balanced, remaining)
        ret._1 == 0
      }
      else {
        val mid = (from + until) / 2
        val ((left_balanced, left_remaining), (right_balanced, right_remaning)) =
          parallel(traverse(from, mid, par_balanced, remaining),
            traverse(mid, until, par_balanced, remaining))

        (left_remaining == 0 && right_remaning == 0 && left_balanced == 0 && right_balanced == 0) ||
          (left_balanced > 0 && right_balanced < 0 && left_remaining + right_remaning == 0)
      }
    }

    reduce(0, chars.length)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
