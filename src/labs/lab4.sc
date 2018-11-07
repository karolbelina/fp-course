// ex 2
// evaluate an expression
// supports addition and negation on integers
sealed trait Expression
case class Value(value: Int) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Negation(expression: Expression) extends Expression

def evaluate(expression: Expression): Int = {
  expression match {
    case Value(value) => value
    case Sum(left, right) => evaluate(left) + evaluate(right)
    case Negation(expr) => -evaluate(expr)
  }
}

// usage
evaluate(Negation(Sum(Value(1), Value(1))))

// ex 3
// define a structure checking if a component of some index exists
// basically a list but whatever
sealed trait Computer
case object Empty extends Computer
case class Component(exists: Boolean, next: Computer) extends Computer

def componentExists(computer: Computer, index: Int): Boolean = {
  def f(x: Computer, i: Int): Boolean = {
    x match {
      case Empty => false
      case Component(exists, next) =>
        if(i == index) exists
        else f(next, i + 1)
    }
  }
  f(computer, 0)
}

// usage
componentExists(Component(true, Component(false, Component(true, Empty))), 2)