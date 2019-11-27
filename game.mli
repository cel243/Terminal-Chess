
type outcome = 
  | Win of Board.color
  | Draw

val play : Board.t -> outcome option