
type outcome = 
  | Win of Board.color
  | Draw

type opponent =
  | Human
  | CPU

val play : Board.t -> opponent -> bool -> outcome option