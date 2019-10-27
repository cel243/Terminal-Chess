(** Determines whether a player's attempted move is legal given 
    the rules of chess and the state of the game  *)

(** [is_legal g c] is [true] if [c] is a legal command given the 
    state of game [g] *)
val is_legal : Board.t -> Command.t -> bool 