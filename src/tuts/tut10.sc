// ex 3
class UnderflowException(msg: String) extends Exception(msg)

class MyQueue[+T] private(private val rep: (List[T], List[T])) {
  def enqueue[S >: T](e: S): MyQueue[S] = {
    rep match {
      case (Nil, _) => new MyQueue((List(e), Nil))
      case (xl, yl) => new MyQueue((xl, e :: yl))
    }
  }

  def dequeue(): MyQueue[T] = {
    rep match {
      case (Nil, _) => this
      case (List(_), yl) => new MyQueue((yl.reverse, Nil))
      case (_ :: tx, yl) => new MyQueue((tx, yl))
    }
  }

  def first(): T = {
    rep match {
      case (h :: _, _) => h
      case (Nil, _) => throw new UnderflowException("MyQueue: first")
    }
  }

  def isEmpty(): Boolean = {
    rep._1 == Nil
  }
}

object MyQueue {
  def apply[T](xs: T*) = new MyQueue[T]((xs.toList, Nil))
  def empty[T] = new MyQueue[T]((Nil,Nil))
}

// ex 4
import scala.collection.mutable
object Copy {
  def copy[T](dest: mutable.Seq[T], src: mutable.Seq[T]): Unit = {
    require(dest.length >= src.length)
    var index = 0
    src.foreach(element => {
      dest.update(index, element)
      index += 1
    })
  }
}