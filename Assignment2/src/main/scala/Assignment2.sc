type Set = Int => Boolean

def contains(s: Set, x: Int): Boolean = s(x)

// Functions for printing infinite sets

def allInRange(s: Set, lo: Int, hi: Int): List[Int] = {
  var elements = List[Int]()

  for (x <- lo to hi)
    if (contains(s, x))
      elements = elements :+ x

  elements
}

def allPositive(s: Set) = allInRange(s, 1, 1000)
def allNegative(s: Set) = allInRange(s, -1000, -1)

def printFirst(s: Set, size: Int) = {
  val pos = allPositive(s)
  val neg = allNegative(s)
  val isZero = contains(s, 0)

  var numOfPos = size / 2
  var ending = " ...}"

  if (pos.length <= size / 2) {
    numOfPos = pos.length
    ending = "}"
  }

  val numZero = if (isZero) 1 else 0
  val rest = size - (numOfPos + numZero)

  var numOfNeg = rest
  var beginning = "{... "

  if (neg.length <= rest) {
    numOfNeg = neg.length
    numOfPos += rest - numOfNeg
    beginning = "{"
  }

  var elements = neg.takeRight(numOfNeg)
  if (isZero) elements = elements ::: List(0)
  elements = elements ::: pos.take(numOfPos)

  println(elements.mkString(beginning, ", ", ending))
}

def printSet(s: Set) = printFirst(s, 10)

// Set operations

def union(s: Set, t: Set): Set =
  (x: Int) => contains(s, x) || contains(t, x)

def intersect(s: Set, t: Set): Set =
  (x: Int) => contains(s, x) && contains(t, x)

def diff(s: Set, t: Set): Set =
  (x: Int) => contains(s, x) && !contains(t, x)

// As a matter of fact, it's the same as union
def filter(s: Set, p: Int => Boolean): Set =
  (x: Int) => contains(s, x) && p(x)

def even = (x: Int) => x % 2 == 0
def odd = (x: Int) => x % 2 != 0
def mult3 = (x: Int) => x % 3 == 0

printSet(even)
// {... -8, -6, -4, -2, 0, 2, 4, 6, 8, 10 ...}

printSet(odd)
// {... -9, -7, -5, -3, -1, 1, 3, 5, 7, 9 ...}

printSet(mult3)
// {... -12, -9, -6, -3, 0, 3, 6, 9, 12, 15 ...}

printSet(union(even, odd))
// {... -4, -3, -2, -1, 0, 1, 2, 3, 4, 5 ...}

printSet(intersect(even, mult3))
// {... -24, -18, -12, -6, 0, 6, 12, 18, 24, 30 ...}

printSet(diff(even, mult3))
// {... -14, -10, -8, -4, -2, 2, 4, 8, 10, 14 ...}

printSet(filter(odd, (x: Int) => x % 5 == 0))
// {... -45, -35, -25, -15, -5, 5, 15, 25, 35, 45 ...}

printSet(filter(odd, even))
// {}

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean =
    if (a > 1000) true
    else if (contains(s, a) && !p(a)) false
    else iter(a + 1)

  iter(-1000)
}

def exists(s: Set, p: Int => Boolean): Boolean =
  !forall(s, (x: Int) => !p(x))

forall(even, (x: Int) => contains(odd, x + 1))
// Boolean = true

forall(even, (x: Int) => x % 10 == 0)
// Boolean = false

exists(even, (x: Int) => x % 10 == 0)
// Boolean = true

def map(s: Set, f: Int => Int): Set =
  (y: Int) => exists(s, (x: Int) => f(x) == y)

def zero = (x: Int) => x == 0
def positive = (x: Int) => x > 0
def negative = (x: Int) => x < 0

def natural = union(positive, zero)
def squares = map(natural, (x: Int) => x * x)

printSet(zero)
// {0}

printSet(positive)
// {1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ...}

printSet(negative)
// {... -10, -9, -8, -7, -6, -5, -4, -3, -2, -1}

printSet(natural)
// {0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ...}

printSet(squares)
// {0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ...}

def fiveInt = (x: Int) => x >= 0 && x <= 5

printSet(fiveInt)
// {0, 1, 2, 3, 4, 5}