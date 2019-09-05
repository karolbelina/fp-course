package ex3

class Pracownik(val nazwisko: String) {
  private var zwolniony: Boolean = false
  Pracownik.liczbaPracownikow += 1

  def zwolnij(): Unit = {
    if(!zwolniony) {
      zwolniony = true
      Pracownik.liczbaPracownikow -= 1
    }
  }

  override def toString: String = nazwisko + ", " + (if(zwolniony) "zwolniony" else "nie zwolniony")

  def liczbaPracownikow: Int = Pracownik.liczbaPracownikow
}

object Pracownik {
  private var liczbaPracownikow = 0
}

object Main {
  def main(args: Array[String]): Unit = {
    val a = new Pracownik("Kowalski")
    val b = new Pracownik("Nowak")
    println(a.liczbaPracownikow)
    a.zwolnij()
    println(a.liczbaPracownikow)
  }
}