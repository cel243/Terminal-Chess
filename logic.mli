(** Determines whether a player's attempted move is legal given 
    the rules of chess and the state of the game  *)

(** the type of the result of a player's attempted move. *)
type res = Legal | Illegal of string | Checkmate | Stalemate | Draw

(** [is_valid_location c i] is true if [c] is a member of the set 
    [{'A',...,'Z'}] and [i] is within the range [1, 8]; otherwise, 
    it is false. *)
val is_valid_location : char -> int -> bool 

(** [step curr dest] is the next value of [curr] in a stepwise
    sweep towards [dest]. *)
val step : int -> int -> int 

(** [is_legal brd c1 c2 c2 i2] is [true] if the current player 
    moving the piece at [c1, i1] to [c2, i2] is a legal move 
    given the current state of the game.  *)
val is_legal : Board.t -> char -> int -> char -> int -> bool * string 

(** [process g c] updates the board with the result of the move if
    it determines the move is legal and returns [Lega], otherwise it returns 
    [Illegal], or [Terminate] if this move ends the game.  *)
val process : Board.t -> Command.t -> res 

(** [king_in_check brd] is [true] if the current player's king is in check, 
    and [false] otherwise.  *)
val king_in_check : Board.t -> bool

(**  [en_passant i1 c2 brd] is [true] if a pawn can be taken by en passant *)
val en_passant : int -> char -> Board.t -> bool