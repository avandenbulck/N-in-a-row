import Player.Player

class Game() {

  var gameState = GameState.Uninitialized
  var nInRowToWin: Int = 0
  private var board: Board = null

  def startNewGame(rows: Int, column: Int, nInRowToWin: Int): Unit ={
    board = new Board(rows, column)
    gameState = GameState.Player1Turn
    this.nInRowToWin = nInRowToWin
  }

  def sinkChecker(column: Int, player: Player): Unit ={
    if(isValidPlayer(player)){

      board.sinkChecker(column, player)
      if(gameState == GameState.Player1Turn)
        gameState = GameState.Player2Turn
      else if(gameState == GameState.Player2Turn)
        gameState = GameState.Player1Turn

      checkWinState()
    }
    else
      throw new IllegalArgumentException(f"Player $player%s tried to play, but gamestate is $gameState%s")
  }

  def isValidPlayer(player: Player): Boolean = {
    if(gameState == GameState.Player1Turn && player == Player.Player1)
      true
    else
      gameState == GameState.Player2Turn && player == Player.Player2
  }

  def checkWinState(): Unit = {
    var playerWon: Player = Player.NoPlayer

    val checkForWinner = (row:Int, column:Int, nextPosition: ((Int, Int)) => (Int, Int)) =>
      if(playerWon == Player.NoPlayer)
        playerWon = checkWinState(row, column, nextPosition)

    (1 to board.rows).foreach(checkForWinner(_, 1, {case(r,c) => (r + 1, c)}))
    (1 to board.columns).foreach(checkForWinner(1, _, {case(r,c) => (r, c + 1)}))

    val allPositions = for(r <- 1 to board.rows;c <- 1 to board.columns) yield (r,c)
    allPositions.foreach({case (r,c) => checkForWinner(r, c, {case(r,c) => (r + 1, c + 1)})})
    allPositions.foreach({case (r,c) => checkForWinner(r, c, {case(r,c) => (r - 1, c + 1)})})

    if(playerWon == Player.Player1)
      gameState = GameState.Player1Won
    if(playerWon == Player.Player2)
      gameState = GameState.Player2Won
    }

  def checkWinState(row: Int, column: Int, nextPosition: ((Int, Int)) => (Int, Int), checkingForPlayer: Player = Player.NoPlayer, sameInARow: Int = 0): Player = {
    if(sameInARow == nInRowToWin)
      checkingForPlayer
    else if (!board.isPositionInBoard(row, column))  {
      Player.NoPlayer
    } else {
      val playerAtPos = board.getCheckerAtPosition(row, column)
      val (newRow, newCol) = nextPosition((row, column))

      if(playerAtPos == checkingForPlayer && playerAtPos != Player.NoPlayer)
        checkWinState(newRow, newCol,nextPosition, checkingForPlayer, sameInARow + 1)
      else
        checkWinState(newRow, newCol,nextPosition, playerAtPos, 1)
    }
  }

  def isGameOver(): Boolean ={
    !(gameState == GameState.Player1Turn || gameState == GameState.Player2Turn)
  }

  override def toString(): String = {
    board.toString()
  }
}
