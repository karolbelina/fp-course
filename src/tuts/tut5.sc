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