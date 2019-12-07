(** This module parses player commands in the chess game *)

(** the type of two locations on the chess board, where the columns
    are labeled A..H from left to right, and the rows are labelled 
    1..8 from bottom to top. 
    Example:
    ('A',4,'B',6) is a pair of locations, where the first
    location is A4 and the second is B6. *)
type locations = char * int * char * int 

(** the type of player commands asking for specific support 
    operations.  *)
type request = 
  | LegalMoves of char * int 
  | LegalMovesIF of char * int * char * int * char * int 
  | UnderAttack 
  | UnderAttackIF of locations
  | CanAttack 
  | CanAttackIF of locations
  | Attackers of char * int 
  | AttackersIF of char * int * char * int * char * int 
  | Suggest of (Board.t -> char * int * char * int)
  | SuggestIF of (Board.t -> char * int * char * int)*locations

(** the type of a player command *)
type t = 
  | Resign 
  | Draw 
  | Help 
  | Captured
  | Move of locations
  | PSupport of request 
  | Log 
  | Save of string 

(** raised when the command isn't one of the expected forms  *)
exception Invalid 

(** [parse f str]  translates [str] into output command. Valid 
    player inputs are detailed in the help menu. Capitalizations and
    white space are ignored. 
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    Requires: [f] is a function that returns a suggested next move. 
    Raises: [Invalid] if [str] is not one of the valid input formats *)
val parse : (Board.t -> char * int * char * int) -> string -> t



