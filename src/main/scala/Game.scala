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
      true
    else
      gameState == GameState.Player2Turn && player == Player.Player2
  }

  def CheckWinState(): Unit = {
    var playerWon: Player = Player.NoPlayer

    for (row <- 1 to board.rows) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateHorizontal(row, 1)
    }

    for (column <- 1 to board.columns) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateVertical(1, column)
    }

    for (row <- 1 to board.rows; column <- 1 to board.columns) {
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateDiagonalLeft(row, column)
      if (playerWon == Player.NoPlayer)
        playerWon = CheckWinStateDiagonalRight(row, column)
    }

    if(playerWon == Player.Player1)
      gameState = GameState.Player1Won
    if(playerWon == Player.Player2)
      gameState = GameState.Player2Won
    }

  def CheckWinStateHorizontal(row: Int, column: Int): Player = {
    var nInRow = 0
    var checkingForPlayer = Player.NoPlayer
    var winningPlayer = Player.NoPlayer
    var currentRow = row
    var currentColumn = column

    while(board.isPositionInBoard(currentRow, currentColumn) && winningPlayer == Player.NoPlayer) {
      val playerAtPos = board.getCheckerAtPosition(currentRow, currentColumn)
      if (playerAtPos == checkingForPlayer && playerAtPos != Player.NoPlayer)
        nInRow += 1
      else {
        nInRow = 1
        checkingForPlayer = playerAtPos
      }
      if (nInRow == nInRowToWin)
        winningPlayer = checkingForPlayer

      currentColumn += 1
    }
    winningPlayer
  }

  def CheckWinStateVertical(row: Int, column: Int): Player = {
    var nInRow = 0
    var checkingForPlayer = Player.NoPlayer
    var winningPlayer = Player.NoPlayer
    var currentRow = row
    var currentColumn = column

    while(board.isPositionInBoard(currentRow, currentColumn) && winningPlayer == Player.NoPlayer) {
      val playerAtPos = board.getCheckerAtPosition(currentRow, currentColumn)
      if (playerAtPos == checkingForPlayer && playerAtPos != Player.NoPlayer)
        nInRow += 1
      else {
        nInRow = 1
        checkingForPlayer = playerAtPos
      }
      if (nInRow == nInRowToWin)
        winningPlayer = checkingForPlayer

      currentRow += 1
    }
    winningPlayer
  }

  def CheckWinStateDiagonalLeft(row: Int, column: Int): Player = {
    var nInRow = 0
    var checkingForPlayer = Player.NoPlayer
    var winningPlayer = Player.NoPlayer
    var currentRow = row
    var currentColumn = column

    while(board.isPositionInBoard(currentRow, currentColumn) && winningPlayer == Player.NoPlayer) {
      val playerAtPos = board.getCheckerAtPosition(currentRow, currentColumn)
      if (playerAtPos == checkingForPlayer && playerAtPos != Player.NoPlayer)
        nInRow += 1
      else {
        nInRow = 1
        checkingForPlayer = playerAtPos
      }
      if (nInRow == nInRowToWin)
        winningPlayer = checkingForPlayer

      currentColumn += 1
      currentRow -= 1
    }
    winningPlayer
  }

  def CheckWinStateDiagonalRight(row: Int, column: Int): Player = {
    var nInRow = 0
    var checkingForPlayer = Player.NoPlayer
    var winningPlayer = Player.NoPlayer
    var currentRow = row
    var currentColumn = column

    while(board.isPositionInBoard(currentRow, currentColumn) && winningPlayer == Player.NoPlayer) {
      val playerAtPos = board.getCheckerAtPosition(currentRow, currentColumn)
      if (playerAtPos == checkingForPlayer && playerAtPos != Player.NoPlayer)
        nInRow += 1
      else {
        nInRow = 1
        checkingForPlayer = playerAtPos
      }
      if (nInRow == nInRowToWin)
        winningPlayer = checkingForPlayer

      currentColumn += 1
      currentRow += 1
    }
    winningPlayer
  }

  def isGameOver(): Boolean ={
    !(gameState == GameState.Player1Turn || gameState == GameState.Player2Turn)
  }

  override def toString(): String = {
    board.toString()
  }
}
