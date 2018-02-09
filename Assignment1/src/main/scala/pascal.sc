def pascal(c: Int, r: Int): Int =
  if ((r == 0) || (r == c)) 1
  else pascal(c - 1, r - 1) + pascal(c - 1, r)

for (c <- 0 to 10) {
  for (r <- 0 to c) {
    print(pascal(c, r) + " ")
  }
  println()
}