// ex 2
// evaluate an expression
// supports addition and negation on integers
sealed trait Expression
case class Value(value: Int) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Negation(expression: Expression) extends Expression

def evaluate(expression: Expression): Int = {
  expression match{
    case Value(value) => value
    case Sum(left, right) => evaluate(left) + evaluate(right)
    case Negation(expr) => -evaluate(expr)
  }
}

// usage
evaluate(Negation(Sum(Value(1), Value(1))))