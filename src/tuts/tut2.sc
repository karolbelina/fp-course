import scala.annotation.tailrec

// ex 2
// calculate the n-th Fibonacci number using regular, as well as tail recursion
def fibonacciA(n: Int): Int = {
  if(n < 0) throw new Exception("fibonacciA: n must be a natural number")
  else if(n == 0) 0
  else if(n == 1) 1
  else fibonacciA(n - 2) + fibonacciA(n - 1)
}

def fibonacciB(n: Int): Int = {
  @tailrec
  def f(n: Int, a: Int, b: Int): Int = {
    if(n < 0) throw new Exception("fibonacciB: n must be a natural number")
    else if(n == 0) a
    else if(n == 1) b
    else f(n - 1, b, b + a)
  }
  f(n, 0, 1)
}

// ex 3
// calculate the cube root of the specified number using the Newton-Raphson method
def root3(a: Double): Double = {
  @tailrec
  def f(x: Double): Double = {
    if(Math.abs(x*x*x - a) <= 10e-15 * Math.abs(a)) x
    else f(x + (a / (x*x) - x) / 3)
  }
  f(if(a > 1) a/3 else a)
}

// ex 5
// check if the first list is a prefix of the second list
def initSegment[A](segment: List[A], list: List[A]): Boolean = {
  (segment, list) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case _ =>
      if(segment.head == list.head) initSegment(segment.tail, list.tail)
      else false
  }
}

// ex 6
// replace the n-th element of a list with the specified value
def replaceNth[A](xs: List[A], n: Int, x: A): List[A] = {
  xs match {
    case Nil => Nil
    case head :: tail =>
      if(n == 0) x :: tail
      else head :: replaceNth(tail, n - 1, x)
  }
}