import Player.Player

class Board(val rows: Int, val columns: Int) {

  var board = Array.fill[Player](rows,columns) {Player.NoPlayer}

  // Returns the owner of the checker at position (row, column)
  def getCheckerAtPosition(row: Int, column: Int) = {
    if(!isPositionInBoard(row, column))
      throw new IllegalArgumentException(f"($row, $column) is not a valid position on board!")
    board(row - 1)(column - 1)
  }

  def isPositionInBoard(row: Int, column: Int): Boolean = {
    return row >= 1 && row <= rows && column >= 1 && column <= columns
  }

  def sinkChecker(column: Int, player: Player): Unit = {
    sinkChecker(rows, column, player)
  }

  private def sinkChecker(row: Int, column: Int, player: Player): Unit = {

    if(getCheckerAtPosition(row, column) != Player.NoPlayer)
      throw new IllegalArgumentException(f"Tried to insert a checker at position ($row,$column) which contains a player's checker.")
    else if(row == 1) // Bottom row
      placeChecker(row, column, player)
    else if (getCheckerAtPosition(row - 1, column) != Player.NoPlayer) // Done sinking
      placeChecker(row, column, player)
    else
      sinkChecker(row - 1, column, player)
  }

  private def placeChecker(column: Int, row: Int, player: Player): Unit ={
    board(column - 1)(row-1) = player
  }

  override def toString(): String = {
    var boardString = ""
    for (row <- rows to 1 by -1; column <- 1 to columns) {
      getCheckerAtPosition(row, column) match {
        case Player.Player1 => boardString += "1"
        case Player.Player2 => boardString += "2"
        case Player.NoPlayer => boardString += "0"
      }

      if(column == columns)
        boardString += "\n"
      else
        boardString += "|"
    }
    return boardString
  }
}
