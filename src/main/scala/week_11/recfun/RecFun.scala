package week_11.recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0) 1
    else if (c % r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def findOpen(idxStart: Int): Boolean = {
      if (idxStart == chars.length) true
      else if (idxStart > chars.length) false
      else if (chars(idxStart) == ')') false
      else if (chars(idxStart) == '(') findOpen(findClose(idxStart + 1, 1))
      else findOpen(idxStart + 1)
    }

    def findClose(idxStart: Int, count: Int): Int ={
      if (count == 0) idxStart
      else if (idxStart >= chars.length) idxStart + 1    // Fail
      else if (chars(idxStart) == ')') findClose(idxStart + 1, count - 1)
      else if (chars(idxStart) == '(') findClose(idxStart + 1, count + 1)
      else findClose(idxStart + 1, count)
    }

    findOpen(0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted(Ordering.Int.reverse)

    if (sortedCoins.isEmpty) 0
    else if (money == 0) 1
    else if (sortedCoins.head > money) countChange(money, sortedCoins.tail)
    else countChange(money - sortedCoins.head, sortedCoins) + countChange(money, sortedCoins.tail)
  }
}
