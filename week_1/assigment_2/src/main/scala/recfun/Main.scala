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
    println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))
    println(countChange(4, List(1, 2)))
    println(countChange(301, List(5, 10, 20, 50, 100, 200, 500)))
    println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))
    println(countChange(300, List(500, 5, 50, 100, 20, 200, 10)))
    println(countChange(0, List(500, 5, 50, 100, 20, 200, 10)))
    println(countChange(30, List()))


  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == -1) 0
    else if (c == 0 || r == 0 || r == c) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def parenthesesBalanceCalculator(currentChar: Char, subChars: List[Char], unbalance: Int): Boolean = {
      if (unbalance < 0) false
      else if (subChars.isEmpty) updateCurrentBalance(currentChar, unbalance) == 0
      else parenthesesBalanceCalculator(subChars.head, subChars.tail, updateCurrentBalance(currentChar, unbalance))
    }

    def updateCurrentBalance(char: Char, currentBalance: Int): Int = {
      if (currentBalance < 0) currentBalance
      else if (char == ')') currentBalance - 1
      else if (char == '(') currentBalance + 1
      else currentBalance
    }

    parenthesesBalanceCalculator(chars.head, chars.tail, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def innerCountChange(money: Int, coins: List[Int]) = {

      if (money == 0) 0
      else {
        val sortedCoins = coins.sortWith(_ < _)
        if (!sortedCoins.exists(_ <= money)) 0
        else {
          calculateCombinationCount(sortedCoins.head, sortedCoins.tail, 0, money, isMainRecursiveBranch = true, "")
        }
      }
    }


    def calculateCombinationCount(currentCoin: Int,
                                  currentCoins: List[Int],
                                  currentCombinationCount: Int,
                                  balance: Int,
                                  isMainRecursiveBranch: Boolean,
                                  coinPath: String): Int = {


      def invokeRecusiveCall(newBalance: Int, newCoinPath: String, recursiveCoins: List[Int]): Int = {
        if (recursiveCoins.isEmpty) 0
        else calculateCombinationCount(recursiveCoins.head, recursiveCoins.tail, 0, newBalance, isMainRecursiveBranch = false, newCoinPath) +
          invokeRecusiveCall(newBalance, newCoinPath, recursiveCoins.tail)
      }

      def choseBalanceCalculation = {
        val newBalance = balance - currentCoin
        val newCoinPath = if (coinPath.isEmpty) currentCoin.toString else coinPath + " + " + currentCoin
        if (newBalance == 0) {
          if (currentCoins.isEmpty) {
            currentCombinationCount + 1
          }
          else {
            if (isMainRecursiveBranch) {
              calculateCombinationCount(currentCoins.head, currentCoins.tail, currentCombinationCount + 1, money, isMainRecursiveBranch, "")
            } else currentCombinationCount + 1
          }
        }
        else {
          if (currentCoins.isEmpty) calculateCombinationCount(currentCoin, currentCoins, currentCombinationCount, newBalance, isMainRecursiveBranch, newCoinPath)
          else {
            calculateCombinationCount(currentCoin, currentCoins, currentCombinationCount, newBalance, isMainRecursiveBranch, newCoinPath) +
              invokeRecusiveCall(newBalance, newCoinPath, currentCoins)
          }
        }
      }

      if (balance >= currentCoin) {
        choseBalanceCalculation
      }
      else
        currentCombinationCount
    }

    innerCountChange(money: Int, coins: List[Int])
  }
}
