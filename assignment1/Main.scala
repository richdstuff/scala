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
    if (c == 0 && r == 0) 1
    else if (c < 0 || c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def bal(x: List[Char], left: Int): Boolean = {
      
      val cur = x.head
      val len = x.length
      if (left < 0) false
      else if (x.isEmpty) left == 0 
      else if (cur == ')' && len == 1) left - 1 == 0
      else if (cur == '(' && len == 1) false
      else if (cur == ')') bal(x.tail, left - 1)
      else if (cur == '(') bal(x.tail, left + 1)
      else if (len == 1) left == 0
      else bal(x.tail, left)
    }

    bal(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(sum: Int, ls: List[Int], tmp: Int): Int = {
      if (tmp < 0) sum
      else count(sum + countChange(tmp, ls.tail), ls, tmp - ls.head)
    }

    if (money == 0 && coins.isEmpty) 1
    else if (money > 0 && coins.isEmpty || money < 0) 0
    else count(0, coins, money)
  }
}
