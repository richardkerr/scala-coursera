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
  def pascal(c: Int, r: Int): Int = {

    def factorial(n: Int): Int = {
      def fact(acc: Int, n: Int): Int = {
        if (n == 0) acc
        else fact(acc * n, n - 1)
      }
      fact(1, n);
    }

    factorial(r) / (factorial(c) * factorial(r - c))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) (open == 0)
      else if (chars.head == '(') loop(chars.tail, open + 1)
      else if (chars.head == ')')
        if (open > 0) loop(chars.tail, open - 1)
        else false
      else loop(chars.tail, open)
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
