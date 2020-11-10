import scala.annotation.tailrec

val chars = "I never (a(dfdf(gg)t)y)be used".toList

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

balance(chars)