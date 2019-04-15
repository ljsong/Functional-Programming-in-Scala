package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("I told him (that it’s not (yet done). (But he wasn’t listening)".toList))
    val coins: List[Int] = List(1, 5, 10, 20, 50, 100)
    println(countChange(721, coins))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || r == c) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(n: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty && n == 0) true
        else if (n != 0 && chars.isEmpty) false
        else if (n < 0) false
        else if (chars.head == '(') balanceIter(n + 1, chars.tail)
        else if (chars.head == ')') balanceIter(n - 1, chars.tail)
        else balanceIter(n, chars.tail)
      }

      balanceIter(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
       if (money == 0 ) 1
       else if (money < 0 || coins.isEmpty) 0
       else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
