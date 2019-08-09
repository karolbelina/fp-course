// ex 1
// simualate a while loop
def whileLoop(condition: =>Boolean)(expression: =>Unit): Unit = {
  if(condition) {
    expression
    whileLoop(condition)(expression)
  }
}

// ex 2
// rewrite the quickSort function from the lecture
def swap[A](tab: Array[A], i: Int, j: Int): Unit = {
  val aux = tab(i)
  tab(i) = tab(j)
  tab(j) = aux
}

def choosePivot[A](tab: Array[A], m: Int, n: Int) = tab((m + n) / 2)

def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) = {
  var i = l
  var j = r
  val pivot = choosePivot(tab, l, r)
  while (i <= j) {
    while (tab(i) < pivot) i += 1
    while (pivot < tab(j)) j -= 1
    if (i <= j) {
      swap(tab, i, j)
      i += 1
      j -= 1
    }
  }
  (i, j)
}

def quick(tab: Array[Int], l: Int, r: Int): Unit =
  if (l < r) {
  val (i, j) = partition(tab, l, r)
  if (j - l < r - i) {
    val _ = quick(tab, l, j)
    quick(tab, i, r)
  }
  else {
    val _ = quick(tab, i, r)
    quick(tab, l, j)
  }
}

def quickSort(tab: Array[Int]): Unit = quick(tab, 0, tab.length - 1)