(** Determines whether a player's attempted move is legal given 
    the rules of chess and the state of the game  *)

(** the type of the result of a player's attempted motion. [Legal] if 
    the move was legal, [Illegal] if not, and [Teminate] if the move results
    in termination of the game.  *)
type res = Legal | Illegal |Terminate 

(** [play_if_legal g c] is [true] if [c] is a legal command given the 
    state of game [g] *)
val play_if_legal : Board.t -> Command.t -> unit 