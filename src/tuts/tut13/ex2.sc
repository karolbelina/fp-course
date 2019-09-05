package ex2

abstract class AbstractPair {
  type A
  type B
  val fstInit: A
  val sndInit: B
  var fst: A
  var snd: B
  override def toString: String = "(" + fst + ", " + snd + ")"
}

object Main {
  def main(args: Array[String]): Unit = {
    val x = new AbstractPair {
      type A = Int
      type B = String
      val fstInit: A = 5
      val sndInit: B = "abc"
      var fst: A = fstInit
      var snd: B = sndInit
    }

    println(x)
    x.fst = 7
    println(x)
    println(x.snd)
  }
}