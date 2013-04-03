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
    if (c < 0 || r < 0 || c > r) throw new NoSuchElementException
    if (c > Int.MaxValue || r > Int.MaxValue) throw new NoSuchElementException("Int out of bounds")

    if (c == 0 || r == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) throw new NoSuchElementException

    def parens(left_counter: Int, right_counter: Int, remain: List[Char]) : Boolean = {
      if (left_counter >= right_counter){
        if (remain.isEmpty && left_counter == right_counter) true
        else {
          if (remain.head == '(') parens(left_counter + 1, right_counter, remain.tail)
          else if (remain.head == ')') parens(left_counter, right_counter + 1, remain.tail)
          else parens(left_counter, right_counter, remain.tail)
        }
      }
      else false
    }
    parens(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
