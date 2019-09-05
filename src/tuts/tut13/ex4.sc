package ex4

class Point(protected var _x: Double = 0.0, protected var _y: Double = 0.0) {
  def x: Double = _x

  def x_=(x: Double): Point = {this._x = x; this}

  def y: Double = _y

  def y_=(y: Double): Point = {this._y = y; this}

  override def toString: String = "[" + x + ", " + y + "]"
}

class Circle(x: Double, y: Double, private var _r: Double = 1.0) extends Point(x, y) {
  def r: Double = _r

  def r_=(r: Double): Circle = {this._r = r; this}

  override def toString: String = "[" + super.toString + ", " + r + "]"
}

class Cylinder(x: Double, y: Double, r: Double, private var _h: Double = 1.0) extends Circle(x, y, r) {
  def h: Double = _h

  def h_=(h: Double): Cylinder = {this._h = h; this}

  override def toString: String = "[" + super.toString + ", " + h + "]"
}

object Main {
  def main(args: Array[String]): Unit = {
    val a = new Point(2, 4)
    println(a)
    println(a.x = 5)

    val b = new Cylinder(1, 1, 5, 4)
    println(b)
    println(b.r = 10)
    println(b.r)
  }
}