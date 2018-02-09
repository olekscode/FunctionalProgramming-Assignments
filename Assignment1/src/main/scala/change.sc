def countChange(money: Int, coins: List[Int]): Int = {
  if (money == 0) return 1
  if (money < 0 || coins.isEmpty) return 0

  // the number of ways to change money using all
  // but the first kind of coin
  val left = countChange(money, coins.tail)

  // the number of ways to change money - coin
  // of the first kind using all kinds of coins
  val right = countChange(money - coins.head, coins)

  left + right
}

countChange(4, List(1, 2))
countChange(100, List(1, 2, 5, 10, 25, 50))