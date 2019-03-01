object GameState extends Enumeration {
  type GameState = Value
  val Uninitialized, Player1Turn, Player2Turn, Player1Won, Player2Won = Value
}
