package ex1

class ParameterizedPair[A, B](val fstInit: A, val sndInit: B) {
  var fst: A = fstInit
  var snd: B = sndInit

  override def toString: String = "(" + fst + ", " + snd + ")"
}

object Main {
  def main(args: Array[String]): Unit = {
    val x = new ParameterizedPair(5, "abc")
    println(x)
    x.fst = 6
    println(x)
    println(x.snd)
  }
}