(** Determines whether a player's attempted move is legal given 
    the rules of chess and the state of the game  *)

(** the type of the result of a player's attempted move. *)
type res = Legal | Illegal of string | Checkmate | Stalemate  

(** [process g c] updates the board with the result of the move if
    it determines the move is legal and returns [Lega], otherwise it returns 
    [Illegal], or [Terminate] if this move ends the game.  *)
val process : Board.t -> Command.t -> res 