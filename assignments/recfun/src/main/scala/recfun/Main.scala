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

    def factorial(n: Int): Int = {
      def fact(acc: Int, n: Int): Int = {
        if (n == 0) acc
        else fact(acc * n, n - 1)
      }
      fact(1, n)
    }

    factorial(r) / (factorial(c) * factorial(r - c))
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
    val sorted = coins.sorted(Ordering[Int].reverse)
    def calc(sortedCoins: List[Int], accum: Int, seq: List[Int]): Int = {
      /*
      if (sortedCoins.isEmpty) 0
      else {
       println(sortedCoins.head + ", " + accum)
       // calc(sortedCoins.tail, accum, combinations) + {
          if (sortedCoins.head + accum > money) {
            println("if")
            0
          }
          else if (sortedCoins.head + accum < money) {
            println("else if")
            combinations + calc(sortedCoins, accum + sortedCoins.head, combinations) + calc(sortedCoins.tail, accum, combinations)
          }
          else {
            println("else")
            1
          }
        //}

      }*/

      if(sortedCoins.isEmpty || accum > money) 0
      else if (accum+sortedCoins.head==money) {
        val wut = seq :+ sortedCoins.head
        println("Found match on " + wut + "(" + accum + "," + sortedCoins.head + ")")
        1 + calc(sortedCoins.tail,accum, seq)
      }
      else {
        println("Checking...(" + seq + "," + accum + "," + sortedCoins.head + ")")
        calc(sortedCoins,accum+sortedCoins.head, seq:+sortedCoins.head) + calc(sortedCoins.tail,accum, seq)
      }
    }
/*
    def loop(sortedCoins: List[Int]): Int = {
      if(sortedCoins.isEmpty) 0
      else {
        val accum = calc(sortedCoins,0,0);
        println("Accum: " + accum)
        accum + loop(sortedCoins.tail)
      }
    }
*/
    calc(sorted,0, Nil);
  }

}
