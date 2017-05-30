import org.scalatest.{Matchers, WordSpec}

class SnakeAndLaddersSpec extends WordSpec with Matchers {

  "Moving Token" should {

    "token can move across the board" in {
      val game = SnakeAndLadders()
      val token = Token("player-1")
      game.placeToken(token)
      token.position shouldBe 1
    }

    "move token" in {
      val game = SnakeAndLadders()
      val token = Token("player-1", 1)
      game.moveToken(token, 3)
      token.position shouldBe 4

    }
    "move token two times" in {
      val game = SnakeAndLadders()
      val token = Token("player-1", 1)
      game.moveToken(token, 3)
      token.position shouldBe 4
      game.moveToken(token, 4)
      token.position shouldBe 8
    }

    "roll dice" in {
      for (i <- 0 to 9999) {
        val die = Dice().roll()
        die should be <= 6
        die should be >= 1
      }
    }

    "roll dice then move" in {
      val game = SnakeAndLadders()
      val token = Token("player-1", 1)
      game.placeToken(token)

      game.moveToken(token, 4)
      token.position shouldBe 5
    }

    "player wins the game" in {
      val game = SnakeAndLadders()
      val token = Token("player-1")
      game.placeToken(token)
      game.moveToken(token, 96)
      game.moveToken(token, 3)
      token.hasWon shouldBe true
    }

    "player doesnt win the game" in {
      val game = SnakeAndLadders()
      val token = Token("player-1")
      game.placeToken(token)
      game.moveToken(token, 96)
      game.moveToken(token, 4)
      token.hasWon shouldBe false
    }
  }

  "Snakes and Ladders" should {

    "snake goes down" in {
      val snake1 = Snake(2, 12)
      val game = SnakeAndLadders(Seq(snake1))
      val token = Token("player-1")
      game.placeToken(token)
      game.moveToken(token, 11)
      token.position shouldBe 2

    }

    "ladder goes up" in {
      val ladder = Ladder(2, 12)
      val game = SnakeAndLadders(ladders = Seq(ladder))
      val token = Token("player-1")
      game.placeToken(token)
      game.moveToken(token, 1)
      token.position shouldBe 12

    }
  }

  "Multiple players" should {

    "determine play order" in {
      for (i <- 0 to 999) {
        val o1 = Dice().roll()
        val o2 = Dice().roll()
        val player1 = Player(o1)
        val player2 = Player(o2)
        if (player1.order > player2.order) {
          val game = SnakeAndLadders(players = Seq(player1, player2))
          game.firstPlayer shouldBe Some(player1)
        } else if (player2.order > player1.order) {
          val game = SnakeAndLadders(players = Seq(player2, player1))
          game.firstPlayer shouldBe Some(player2)
        } else {
          val game = SnakeAndLadders()
          game.firstPlayer shouldBe None
        }
      }

    }
  }
}
