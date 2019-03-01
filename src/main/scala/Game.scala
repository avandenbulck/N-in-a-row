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

      CheckWinState()
    }
    else
      throw new IllegalArgumentException(f"Player $player%s tried to play, but gamestate is $gameState%s")
  }

  def isValidPlayer(player: Player): Boolean = {
    if(gameState == GameState.Player1Turn && player == Player.Player1)
      return true
    else if(gameState == GameState.Player2Turn && player == Player.Player2)
      return true
    else
      return false
  }

  def CheckWinState(): Unit = {
    var playerWon: Player = Player.NoPlayer

    for (row <- 1 to board.rows) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateHorizontal(row, 1, Player.NoPlayer, 0)
    }

    for (column <- 1 to board.columns) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateVertical(1, column, Player.NoPlayer, 0)
    }

    for (row <- 1 to board.rows; column <- 1 to board.columns) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateDiagonalLeft(row, column, Player.NoPlayer, 0)
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateDiagonalRight(row, column, Player.NoPlayer, 0)
    }

    playerWon match {
      case Player.Player1 => gameState = GameState.Player1Won
      case Player.Player2 => gameState = GameState.Player2Won
      case _ =>
    }
  }

  // TODO: CheckWinStateHorizontal, CheckWinStateVertical, CheckWinStateDiagonalLeft and CheckWinStateDiagonalRight contain too much duplicate code. Refactor
  def CheckWinStateHorizontal(row: Int, column: Int, player: Player.Value, nInRow: Int): Player = {
      if(nInRow == nInRowToWin)
        return player;
      else if (!board.isPositionInBoard(row, column)) // End of board
        return Player.NoPlayer
      else {
        board.getCheckerAtPosition(row, column) match {
          case p if p == player && p != Player.NoPlayer =>
            return CheckWinStateHorizontal(row, column + 1, player, nInRow + 1)
          case p =>
            return CheckWinStateHorizontal(row, column + 1, p, 1)
        }
      }
  }

  def CheckWinStateVertical(row: Int, column: Int, player: Player.Value, nInRow: Int): Player = {
    if(nInRow == nInRowToWin)
      return player;
    else if (!board.isPositionInBoard(row, column)) // End of board
      return Player.NoPlayer
    else {
      board.getCheckerAtPosition(row, column) match {
        case p if p == player && p != Player.NoPlayer =>
          return CheckWinStateVertical(row + 1, column, player, nInRow + 1)
        case p =>
          return CheckWinStateVertical(row + 1, column, p, 1)
      }
    }
  }

  def CheckWinStateDiagonalLeft(row: Int, column: Int, player: Player.Value, nInRow: Int): Player = {
    if(nInRow == nInRowToWin)
      return player;
    else if (!board.isPositionInBoard(row, column)) // End of board
      return Player.NoPlayer
    else {
      board.getCheckerAtPosition(row, column) match {
        case p if p == player && p != Player.NoPlayer =>
          return CheckWinStateDiagonalLeft(row - 1, column + 1, player, nInRow + 1)
        case p =>
          return CheckWinStateDiagonalLeft(row - 1, column + 1, p, 1)
      }
    }
  }

  def CheckWinStateDiagonalRight(row: Int, column: Int, player: Player.Value, nInRow: Int): Player = {
    if(nInRow == nInRowToWin)
      return player;
    else if (!board.isPositionInBoard(row, column)) // End of board
      return Player.NoPlayer
    else {
      board.getCheckerAtPosition(row, column) match {
        case p if p == player && p != Player.NoPlayer =>
          return CheckWinStateDiagonalRight(row + 1, column + 1, player, nInRow + 1)
        case p =>
          return CheckWinStateDiagonalRight(row + 1, column + 1, p, 1)
      }
    }
  }

  // TODO: Checking a variable for multiple possibilities can be replaced with pattern matching
  def isGameOver(): Boolean ={
    return !(gameState == GameState.Player1Turn || gameState == GameState.Player2Turn)
  }

  override def toString(): String = {
    return board.toString()
  }
}
