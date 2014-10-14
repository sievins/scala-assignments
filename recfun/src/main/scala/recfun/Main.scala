package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException
    else if (c == 0 || r == c) 1
    else if (c ==1 || c == r-1) r
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def balanceItr(openParen: Int, chars: List[Char]): Boolean =
      if (openParen < 0) false
      else if (chars.isEmpty) openParen == 0
      else if (chars.head == '(') balanceItr(openParen+1, chars.tail)
      else if (chars.head == ')') balanceItr(openParen-1, chars.tail)
      else balanceItr(openParen, chars.tail)
    
    balanceItr(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
