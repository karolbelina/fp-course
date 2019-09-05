// ex 1
class Time1(private var h: Int) {
  if (h < 0) h = 0

  def hour: Int = h

  def hour_=(newHour: Int) {
    if (newHour < 0) h = 0
    else h = newHour
  }
}

object Time1 {
  def apply(hour: Int) = new Time1(hour)
}

// ex 2
class Time2a(private var h: Int, private var m: Int) {
  require(0 <= h && h < 24)
  require(0 <= m && m < 60)

  def hour: Int = h

  def hour_=(newHour: Int) {
    require(0 <= newHour && newHour < 24)
    h = newHour
  }

  def minute: Int = m

  def minute_=(newMinute: Int) {
    require(0 <= newMinute && newMinute < 60)
    m = newMinute
  }

  def before(other: Time2a): Boolean =
    h < other.h || (h == other.h && m < other.m)
}

class Time2b(h: Int, m: Int) {
  require(0 <= h && h < 24)
  require(0 <= m && m < 60)
  private var minutesAfterMidnight = h * 60 + m

  def hour: Int = minutesAfterMidnight / 60

  def hour_=(newHour: Int) {
    require(0 <= newHour && newHour < 24)
    minutesAfterMidnight = newHour * 60 + minute
  }

  def minute: Int = minutesAfterMidnight % 60

  def minute_=(newMinute: Int) {
    require(0 <= newMinute && newMinute < 60)
    minutesAfterMidnight = hour + newMinute
  }

  def before(other: Time2b): Boolean =
    minutesAfterMidnight < other.minutesAfterMidnight
}

// ex 3
class Pojazd(val producent: String, val model: String, val rok: Int = -1, var rejestracja: String = "") {
  def this(producent: String, model: String, rejestracja: String) {
    this(producent, model, -1, rejestracja)
  }
}

object Test extends App {
  new Pojazd("Fiat", "126p", 2016, "ESI123")
  new Pojazd("Fiat", "126p", 2016)
  new Pojazd("Fiat", "126p", "ESI123")
  new Pojazd("Fiat", "126p")
}

// ex 4
object UzycieWyjatkow {
  def main(args: Array[String]) {
    try {
      metoda1()
    } catch {
      case e: Exception =>
        System.err.println(e.getMessage + "\n")
        e.printStackTrace()
    }
  }

  def metoda1() {
    metoda2()
  }

  def metoda2() {
    metoda3()
  }

  def metoda3() {
    throw new Exception("Wyjatek zgloszony w metoda3")
  }
}

/*
Wyjatek zgloszony w metoda3

java.lang.Exception: Wyjatek zgloszony w metoda3
at c9.z4.UzycieWyjatkow$.metoda3(UzycieWyjatkow.scala:22)
at c9.z4.UzycieWyjatkow$.metoda2(UzycieWyjatkow.scala:18)
at c9.z4.UzycieWyjatkow$.metoda1(UzycieWyjatkow.scala:14)
at c9.z4.UzycieWyjatkow$.main(UzycieWyjatkow.scala:6)
at c9.z4.UzycieWyjatkow.main(UzycieWyjatkow.scala)
 */