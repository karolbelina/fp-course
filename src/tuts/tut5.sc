// ex 1
// repeat elements in a stream k times
def lrepeat[A](k: Int, xs: Stream[A]): Stream[A] = {
  def f(n: Int, x: A, xs: Stream[A]): Stream[A] = {
    if(n > 0) x #:: f(n - 1, x, xs)
    else if(n == 0) lrepeat(k, xs)
    else throw new Exception("negative repetition count")
  }
  xs match {
    case Stream.Empty => Stream.Empty
    case h #:: t => f(k, h, t)
  }
}

// ex 2
// define a fibonacci sequence using streams
val fibs: Stream[Int] = {
  def f(a: Int, b: Int): Stream[Int] = a #:: f(b, a + b)
  f(0, 1)
}

// definition of a lazy binary tree for excercise 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

// ex 3
// generate a stream containg every element of the potentially infinite lazy binary tree
def lBTtoStream[A](tree: lBT[A]): Stream[A] = {
  def f(queue: List[lBT[A]]): Stream[A] = {
    queue match {
      case Nil => Stream.Empty
      case LEmpty :: t => f(t)
      case LNode(v, lf, rf) :: t => v #:: f(t ::: List(lf(), rf()))
    }
  }
  f(List(tree))
}

// generate an infinite lazy binary tree with a root of value n and subtrees of value 2*n and 2*n+1
def lTree(n: Int): lBT[Int] = LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))