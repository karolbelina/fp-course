package ex5

object ex5 {
  def wordCounter(text: String): scala.collection.mutable.Map[String, Int] = {
    text.split(" ").foldLeft(scala.collection.mutable.Map.empty[String, Int]) {
      (map, word) => map += (word -> (map.getOrElse(word, 0) + 1))
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(ex5.wordCounter("ala ma kot a kot ma ala"))
  }
}