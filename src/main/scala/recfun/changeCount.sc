

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

  countChangeRecursively(10, List(5,3,2))
