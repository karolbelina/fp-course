// definition of a binary tree for excercises 3 and 4

// ex 3
// define a specific function using foldLeft
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

// an example tree
val tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

// ex 3
// return a list of nodes in order they were visited doing a breadth-first search on a tree
def breadthBT[A](tree: BT[A]): List[A] = {
  def f(queue: List[BT[A]]): List[A] = {
    queue match {
      case Nil => Nil
      case Empty :: t => f(t)
      case Node(v, l, r) :: t => v :: f(t ::: List(l, r))
    }
  }
  f(List(tree))
}

// ex 4
// return the internal and external path length of an extended binary tree
def internalPathLength[A](tree: BT[A]): Int = {
  def f(depth: Int, tree: BT[A]): Int = {
    tree match {
      case Empty => 0
      case Node(_, l, r) => depth + f(depth + 1, l) + f(depth + 1, r)
    }
  }
  f(0, tree)
}

def externalPathLength[A](tree: BT[A]): Int = {
  def f(depth: Int, tree: BT[A]): Int = {
    tree match {
      case Empty => depth
      case Node(_, l, r) => f(depth + 1, l) + f(depth + 1, r)
    }
  }
  f(0, tree)
}

// definition of a graph for excercise 5
sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

// an example graph
val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0,2,4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0,2)
    case n => throw new Exception("Graph g: node " + n + " doesn't exist")
  }
)

// ex 5
// return a list of nodes in order they were visited doing a depth-first search on a graph
def depthSearch[A](graph: Graph[A], node: A): List[A] = {
  def f(stack: List[A], visited: List[A]): List[A] = {
    stack match {
      case Nil => Nil
      case h :: t => if(visited contains h) f(t, visited)
      else h :: f(graph.succ(h) ::: t, h :: visited)
    }
  }
  f(List(node), Nil)
}