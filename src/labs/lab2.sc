import scala.annotation.tailrec

// ex 2
// calculate the n-th Fibonacci number using tail recursion
def fibonacci(n: Int): Int = {
  @tailrec
  def f(n: Int, a: Int, b: Int): Int = {
    if(n < 0) throw new Exception("fibonacci: n must be a natural number")
    else if(n == 0) a
    else if(n == 1) b
    else f(n - 1, b, b + a)
  }
  f(n, 0, 1)
}

// ex 3
// separate the main list into three sublists
// the first one must contain even, the second one odd and positive, the third one odd and negative numbers only
def separate(xs: List[Int]): (List[Int], List[Int], List[Int]) = {
  @tailrec
  def f(xs: List[Int], ys: (List[Int], List[Int], List[Int])): (List[Int], List[Int], List[Int]) = {
    val (l1, l2, l3) = ys
    xs match {
      case Nil => ys
      case h :: t =>
        if(h % 2 == 0) f(t, (h :: l1, l2, l3))
        else if(h > 0) f(t, (l1, h :: l2, l3))
        else f(t, (l1, l2, h :: l3))
    }
  }
  f(xs, (Nil, Nil, Nil))
}

// ex 4
// concatenate two lists, but the elements must alternate
def mix[A](xs: List[A], ys: List[A]): List[A] = {
  (xs, ys) match {
    case (_, Nil) => xs
    case (Nil, _) => ys
    case (h1 :: t1, h2 :: t2) => h1 :: (h2 :: mix(t1, t2))
  }
}

// ex 5
// get 10 largest integers from a list
// TODO: actually make this work
def largest(xs: List[Int]): List[Int] = {
  def replaceNth(xs: List[Int], i: Int, x: Int): List[Int] = {
    xs match {
      case Nil => Nil
      case head :: tail =>
        if(i == 0) x :: tail
        else head :: replaceNth(tail, i - 1, x)
    }
  }

  def minIndex(xs: List[Int]): Int = {
    @tailrec
    def f(xs: List[Int], i: Int, n: Int): Int = {
      xs match {
        case Nil => 0
        case List(x: Int) => i
        case x :: y :: tail => if(x < y) f(x :: tail, i, n + 1) else f(y :: tail, n + 1, n + 1)
      }
    }
    f(xs, 0, 0)
  }

  @tailrec
  def f(xs: List[Int], ys: List[Int]): List[Int] = {
    xs match {
      case Nil => ys
      case h :: t =>
        if(ys.length < 10) f(t, h :: ys)
        else f(t, replaceNth(ys, minIndex(ys), h))
    }
  }
  f(xs, Nil)
}