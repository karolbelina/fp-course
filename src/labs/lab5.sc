// ex 2
// make a specific operation on each pair of elements from two separate streams
def loperation[A](xs: Stream[A], ys: Stream[A], default: A, operation: (A, A) => A): Stream[A] = {
  (xs, ys) match {
    case (hx #:: tx, hy #:: ty) => operation(hx, hy) #:: loperation(tx, ty, default, operation)
    case (hx #:: tx, Stream.Empty) => operation(hx, default) #:: loperation(tx, Stream.Empty, default, operation)
    case (Stream.Empty, hy #:: ty) => operation(default, hy) #:: loperation(Stream.Empty, ty, default, operation)
    case (Stream.Empty, Stream.Empty) => Stream.Empty
  }
}

// tests
loperation(Stream(1, 2, 3, 4), Stream.from(0), 0, (a: Int, b: Int) => a + b).take(10).toList
loperation(Stream("a", "b", "c"), Stream("c", "a"), "", (a: String, b: String) => a + b).toList
loperation(Stream.from(2, 2), Stream.from(0), 0, (a: Int, b: Int) => a * b).take(10).toList