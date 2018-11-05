// ex 1
// calculate the sum of all integers in the list
def sum(xs: List[Int]): Int = {
  xs match {
    case Nil => 0
    case x :: tail => x + sum(tail)
  }
}

// ex 3
// filter out all integers in a list lower or equal than the specified integer
def filter(xs: List[Int], n: Int): List[Int] = {
  xs match {
    case Nil => Nil
    case x :: tail => if(x > n) x :: filter(tail, n) else filter(tail, n)
  }
}

// ex 5
// count how many chars in a list are different than the specified one
def countDifferent(s: List[Char], c: Char): Int = {
  s match {
    case h :: tail => if(h != c) countDifferent(tail, c) + 1 else countDifferent(tail, c)
    case Nil => 0
  }
}