// ex 1
// flatten the list of lists by one level
def flatten[A](xss: List[List[A]]): List[A] = {
  if(xss == Nil) Nil
  else xss.head ::: flatten(xss.tail)
}

// ex 2
// count the occurences of an element in a list
def count[A](x: A, xs: List[A]): Int = {
  if(xs == Nil) 0
  else if(xs.head == x) count(x, xs.tail) + 1 else count(x, xs.tail)
}

// ex 3
// return a list in which the same element is repeated n number of times
def replicate[A](x: A, n: Int): List[A] = {
  if(n < 0) throw new Exception("negative number of repetitions")
  else if(n == 0) Nil
  else x :: replicate(x, n - 1)
}

// ex 4
// square each element of a list
def sqrList(xs: List[Int]): List[Int] = {
  if(xs == Nil) Nil
  else xs.head * xs.head :: sqrList(xs.tail)
}

// ex 5
// check if the specified list is a palindrome
def palindrome[A](xs: List[A]): Boolean = {
  val len = xs.length
  def f(n: Int): Boolean = {
    if(n == 0) true
    else if(xs(len - n) == xs(n - 1)) f(n - 1)
    else false
  }
  f(len / 2)
}

// ex 6
// calculate the length of a list
def listLength[A](xs: List[A]): Int = {
  if(xs == Nil) 0
  else listLength(xs.tail) + 1
}