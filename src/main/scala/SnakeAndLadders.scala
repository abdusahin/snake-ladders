import scala.util.Random

class SnakeAndLadders(val snakes: Seq[Snake], val ladders: Seq[Ladder], val players: Seq[Player]) {
  var nextPlayer = players.headOption

  def squares = 100

  def placeToken(token: Token) = token.position = 1

  def moveToken(token: Token, spaces: Int) = {
    if (nextPlayer.contains(token.player)) {

      token.position = token.position + spaces
      val position = snakes.collectFirst {
        case snake if snake.head == token.position =>
          snake.tail
      }
      position.foreach(token.position = _)
      val lposition = ladders.collectFirst {
        case ladder if ladder.tail == token.position =>
          ladder.head
      }
      lposition.foreach(token.position = _)

      if (token.position == squares)
        token.hasWon = true
      else {
        val idx = players.indexOf(token.player)
        if (idx < players.length - 1)
          nextPlayer = Some(players(idx + 1))
        else
          nextPlayer = players.headOption

      }
    }
  }

}

class Dice {
  def roll() = Random.nextInt(6) + 1
}

object Dice {
  def apply(): Dice = new Dice()
}

object SnakeAndLadders {
  def apply(snakes: Seq[Snake] = Seq.empty, ladders: Seq[Ladder] = Seq.empty, players: Seq[Player] = Seq.empty) =
    new SnakeAndLadders(snakes, ladders, players)
}

case class Token(player: Player, var position: Int = 0, var hasWon: Boolean = false)

case class Snake(tail: Int, head: Int)

case class Ladder(tail: Int, head: Int)

case class Player(val order: Int = 0)
