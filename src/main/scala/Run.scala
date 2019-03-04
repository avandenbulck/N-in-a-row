object Run {

  def main(args: Array[String]) {

    val game = new Game()
    game.startNewGame(6,6,4)

    var inputColumn = 0
    var currentPlayer = Player.Player1

    println(game)
    while ({inputColumn = scala.io.StdIn.readInt(); inputColumn !=  0 && !game.isGameOver()}) {

      game.sinkChecker(inputColumn, currentPlayer)

      currentPlayer = currentPlayer match {
        case Player.Player1 => Player.Player2
        case Player.Player2 => Player.Player1
      }
      println(game)

      if(game.isGameOver())
        print("Game over: " + game.gameState)
    }
  }
}
