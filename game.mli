
type outcome = 
  | Win of Board.color
  | Draw

val play : unit -> outcome option