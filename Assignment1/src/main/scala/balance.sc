import scala.annotation.tailrec

def timeit[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

def balanceIter(chars: List[Char]): Boolean = {
  var count = 0

  for (c <- chars) {
    if (c == '(') count += 1
    else if (c == ')') count -= 1

    if (count < 0) return false
  }
  count == 0
}

def balance(chars: List[Char]): Boolean = {

  @tailrec
  def balanceStep(chars: List[Char], count: Int): Boolean =
    if (count < 0)
      false
    else if (chars.isEmpty)
      count == 0
    else if (chars.head == '(')
      balanceStep(chars.tail, count + 1)
    else if (chars.head == ')')
      balanceStep(chars.tail, count - 1)
    else
      balanceStep(chars.tail, count)

  balanceStep(chars, 0)
}

// Balanced string
val str1 = "he((ll)o(wo(((())))r(ld))".toList

// Too many opened parentheses
val str2 = "he(((ll)o(wo(((())))r(ld))".toList

// Too many closed parentheses
val str3 = "he((ll)o(wo(((())))r(ld)))".toList

// Iterative approach
timeit { balanceIter(str1) }
timeit { balanceIter(str2) }
timeit { balanceIter(str3) }

// Recursive approach
timeit { balance(str1) }
timeit { balance(str2) }
timeit { balance(str3) }