// ex 2
// make a specific operation on each pair of elements from two separate streams
def streamOperation[A](xs: Stream[A], ys: Stream[A], default: A, operation: (A, A) => A): Stream[A] = {
  (xs, ys) match {
    case (hx #:: tx, hy #:: ty) => operation(hx, hy) #:: streamOperation(tx, ty, default, operation)
    case (hx #:: tx, Stream.Empty) => operation(hx, default) #:: streamOperation(tx, Stream.Empty, default, operation)
    case (Stream.Empty, hy #:: ty) => operation(default, hy) #:: streamOperation(Stream.Empty, ty, default, operation)
    case (Stream.Empty, Stream.Empty) => Stream.Empty
  }
}

// tests
streamOperation(Stream(1, 2, 3, 4), Stream.from(0), 0, (a: Int, b: Int) => a + b).take(10).toList
streamOperation(Stream("a", "b", "c"), Stream("c", "a"), "", (a: String, b: String) => a + b).toList
streamOperation(Stream.from(2, 2), Stream.from(0), 0, (a: Int, b: Int) => a * b).take(10).toList