import scala.annotation.tailrec

// ex 2
// define functions curry3 and uncurry3
def curry3[A,B,C,D](f: (A, B, C) => D) = (x: A) => (y: B) => (z: C) => f(x, y, z)
// curry3: [A,B,C,D]((A, B, C) => D) => A => (B => (C => D))

def uncurry3[A,B,C,D](f: A => B => C => D) = (x: A, y: B, z: C) => f(x)(y)(z)
// uncurry3: [A,B,C,D](A => (B => (C => D))) => (A, B, C) => D

// ex 3
// define a specific function using foldLeft
def sumProd(xs: List[Int]): (Int, Int) = (xs foldLeft (0, 1)) ((acc, x) => (acc._1 + x, acc._2 * x))

// ex 5
// insertion sort
def insertionsort[A](pred: (A, A) => Boolean, xs: List[A]): List[A] = {
  def insert(x: A, xs: List[A]): List[A] = {
    xs match {
      case Nil => List(x)
      case h :: t =>
        if(pred(h, x)) h :: insert(x, t)
        else x :: xs
    }
  }
  (xs foldLeft List[A]()) ((acc, x) => insert(x, acc))
}

// usage
insertionsort((x: Int, y: Int) => x < y, List(5, 3, 8, 6, 2, 9, 7, 6, 1))

// merge sort
def mergesort[A](pred: (A, A) => Boolean, xs: List[A]): List[A] = {
  def split(xs: List[A]): (List[A], List[A]) = {
    @tailrec
    def f(xs: (List[A], List[A]), n: Int): (List[A], List[A]) = {
      val (l, r) = xs
      if(n == 0) (l.reverse, r)
      else f((r.head :: l, r.tail), n - 1)
    }
    f((Nil, xs), xs.length / 2)
  }
  def merge(xs: List[A], ys: List[A]): List[A] = {
    (xs, ys) match {
      case (Nil, s) => s
      case (s, Nil) => s
      case (hx :: tx, hy :: ty) =>
        if(pred(hx, hy)) hx :: merge(tx, ys)
        else hy :: merge(xs, ty)
    }
  }
  def f(xs: List[A]): List[A] = {
    xs match {
      case Nil => Nil
      case List(x) => List(x)
      case s => val (left, right) = split(s)
        merge(f(left), f(right))
    }
  }
  f(xs)
}

// usage
mergesort((x: Int, y: Int) => x <= y, List(5, 3, 8, 6, 2, 9, 7, 6, 1))

// check the stability
val xs = List((5, 'a'), (3, 'a'), (5, 'b'), (6, 'a'), (2, 'a'), (9, 'a'), (6, 'b'), (2, 'b'), (6, 'c'))
val pred = (x: (Int, Char), y: (Int, Char)) => x._1 <= y._1
insertionsort(pred, xs)
mergesort(pred, xs)