type Set = Int => Boolean

def contains(s: Set, x: Int): Boolean = s(x)

// Functions for printing infinite sets

def printFirst(s: Set, size: Int) = {
  var elements = List[Int]()
  var x = 0

  while (elements.length < size && x < 1000) {
    if (contains(s, x))
      elements = elements :+ x

    x += 1
  }

  val ending = if (x == 1000) "}" else ", ... }"
  println(elements.mkString("{", ", ", ending))
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
// {0, 2, 4, 6, 8, 10, 12, 14, 16, 18, ... }

printSet(odd)
// {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, ... }

printSet(union(even, odd))
// {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ... }

printSet(intersect(even, mult3))
// {0, 6, 12, 18, 24, 30, 36, 42, 48, 54, ... }

printSet(diff(even, mult3))
// {2, 4, 8, 10, 14, 16, 20, 22, 26, 28, ... }

printSet(filter(odd, (x: Int) => x % 5 == 0))
// {5, 15, 25, 35, 45, 55, 65, 75, 85, 95, ... }

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
def natural = diff(union(odd, even), zero)
def squares = map(natural, (x: Int) => x * x)

printSet(natural)
// {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ... }

printSet(squares)
// {1, 4, 9, 16, 25, 36, 49, 64, 81, 100, ... }

def fiveInt = (x: Int) => x >= 0 && x <= 5

printSet(fiveInt)
// {0, 1, 2, 3, 4, 5}