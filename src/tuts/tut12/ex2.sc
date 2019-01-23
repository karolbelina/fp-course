import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Game {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("System")
    val numberOfPlayers = 3

    val players = new Array[ActorRef](numberOfPlayers)

    for(i <- 0 until numberOfPlayers) {
      players(i) = system.actorOf(Props(classOf[Player], i, players))
    }

    players(0) ! Ball(0)
  }
}

case class Ball(count: Int)

class Player(val num: Int, val players: Array[ActorRef]) extends Actor {
  val random = new scala.util.Random

  override def receive: Receive = {
    case Ball(count) =>
      val otherIndex = (num + 1 + random.nextInt(players.length - 1)) % players.length
      println("Player " + (num + 1) + " throws the ball #" + (count + 1))
      players(otherIndex) ! Ball(count + 1)
  }
}