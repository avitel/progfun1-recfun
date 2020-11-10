package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int ={
    def getPascalRow(r:Int):Vector[Int]={
      def createNextRow(row:Vector[Int]):Vector[Int]={
        val rowLeft = 0 +: row
        val rowRight = row :+ 0
        (rowLeft zip rowRight).map(tuple => tuple._1 + tuple._2)
      }
      @tailrec
      def getRowRecursively(row:Vector[Int]):Vector[Int] = {
        if (row.size == r+1)
          row
        else
          getRowRecursively(createNextRow(row))
      }
      getRowRecursively(Vector(1))
    }
    getPascalRow(r)(c)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def getBalanceRecursively(chars: List[Char], level: Int):Boolean={
      if (chars.isEmpty)
        level==0
      else if (level<0)
        false
      else if (chars.head == '(')
        getBalanceRecursively(chars.tail, level+1)
      else if (chars.head == ')')
        getBalanceRecursively(chars.tail, level-1)
      else
        getBalanceRecursively(chars.tail, level)
    }
    getBalanceRecursively(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeRecursively(money:Int, coins: List[Int]): Int = {
      if (coins.size == 1)
        if (money % coins.head == 0)
          1
        else
          0
      else{
        val majorDenominate = coins.head
        val maxCount = money / majorDenominate
        (0 to maxCount).map(i => {
          val rest = money - i * majorDenominate
          val possible = countChangeRecursively(rest, coins.tail)
          possible
        }).sum
      }
    }
    countChangeRecursively(money, coins.sorted.reverse)
  }
}
