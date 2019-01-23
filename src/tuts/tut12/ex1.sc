import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object PingPong {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("System")
    val player1 = system.actorOf(Player.props("Ping"))
    val player2 = system.actorOf(Player.props("Pong"))

    player2 ! Player.Serve(10, player1)

    system.terminate
  }
}

class Player(val sound: String) extends Actor {
  override def receive: Receive = {
    case Player.Serve(n, otherPlayer) =>
      otherPlayer ! Player.Ball(n)
    case Player.Ball(n) =>
      if(n > 0) {
        println(sound)
        sender ! Player.Ball(n - 1)
      }
  }
}

object Player {
  def props(sound: String): Props = Props(classOf[Player], sound)
  case class Serve(n: Int, otherPlayer: ActorRef)
  case class Ball(n: Int)
}