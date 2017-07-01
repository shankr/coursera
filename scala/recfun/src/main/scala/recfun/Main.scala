package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("(()".toList))

    println(countChange(10, List(1, 5)));
    println(countChange(3, List(1, 5)));
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = 
      if (r < 2) 1
      else if (c == 0  || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def imbalance(chars: List[Char], depth: Int): Int = 
        if (depth < 0) depth
	else if (chars.isEmpty) depth
	else if (chars.head == '(') imbalance(chars.tail, depth + 1)
        else if (chars.head == ')') imbalance(chars.tail, depth - 1)
        else imbalance(chars.tail, depth)

      imbalance(chars, 0) == 0
    }  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = 
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail) 
    }

