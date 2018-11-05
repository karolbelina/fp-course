import scala.annotation.tailrec

// ex 2
// convert a list of binary digits into a single decimal integer
def binaryToDecimal(xs: List[Int]): Int = {
  def pow(a: Int, b: Int): Int = {
    @tailrec
    def f(i: Int, acc: Int): Int = {
      if(i == b) acc
      else f(i + 1, acc * a)
    }
    if(b >= 0) f(0, 1)
    else throw new Exception("negative exponent")
  }
  @tailrec
  def f(xs: List[Int], i: Int, x: Int): Int = {
    xs match {
      case Nil => x
      case h :: t =>
        if(h == 0 || h == 1) f(t, i + 1, x + (if(h == 1) pow(2, i) else 0))
        else throw new Exception("expected 0 or 1")
    }
  }
  f(xs.reverse, 0, 0)
}

// ex 3
// check if three points on a plane make up a triangle
// basically check if the area of such a triangle is not zero
def validTriangle(p1: (Int, Int), p2: (Int, Int), p3: (Int, Int)): Boolean = {
  p1._1 * (p2._2 - p3._2) + p2._1 * (p3._2 - p1._2) + p3._1 * (p1._2 - p2._2) != 0
}