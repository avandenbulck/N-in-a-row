object Run {

  def main(args: Array[String]) {

    val game = new Game()
    game.startNewGame(4,4,2)

    var input: String = null
    var currentPlayer = Player.Player1;

    println(game)
    while ({input = scala.io.StdIn.readLine(); input != "" && !game.isGameOver()}) {

      val inputColumn = input.toInt
      game.sinkChecker(inputColumn, currentPlayer)

      currentPlayer match{
        case Player.Player1 => currentPlayer = Player.Player2
        case Player.Player2 => currentPlayer = Player.Player1
      }
      println(game)

      if(game.isGameOver())
        print("Game over: " + game.gameState)
    }
  }
}
