package recfun

import common._
import scala.util.Sorting

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

    def myfactorial(n: Int): Int = {
      def loop(acc: Int, n: Int): Int = {
        if (n == 0) acc
        else loop(acc * n, n - 1)
      }
      loop(1, n)
    }

    (myfactorial(r) / (myfactorial(c) * myfactorial(r - c)))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], open: Int): Boolean = {
      def getAdjust(char: Char): Int = {
        char match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
      }

      if (chars.isEmpty) open == 0
      else open >= 0 && loop(chars.tail, open + getAdjust(chars.head))

    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def calc(sortedCoins: List[Int], accum: Int): Int = {
      if(sortedCoins.isEmpty || accum > money) 0
      else if (accum+sortedCoins.head==money) 1 + calc(sortedCoins.tail,accum)
      else calc(sortedCoins,accum+sortedCoins.head) + calc(sortedCoins.tail,accum)
    }

    calc(coins.sorted(Ordering[Int].reverse),0)
  }

}
