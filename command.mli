(** This module parses player commands in the chess game *)

(** the type of two locations on the chess board, where the columns
    are labeled A..H from left to right, and the rows are labelled 
    1..8 from bottom to top. 
    Example:
    ('A',4,'B',6) is a pair of locations, where the first
    location is A4 and the second is B6. *)
type locations = char * int * char * int 

(** the type of a player command *)
type t = 
  | Resign 
  | Draw 
  | Help 
  | Captured
  | Move of locations

(** raised when the command isn't one of the expected forms  *)
exception Invalid 

(** [parse str]  translates player input and outputs commands. 
    If the player is not trying to move a piece, the first word 
    of their input must be "quit" or "draw," and the rest of the phrase must
    be empty.
    Otherwise, the player input must be of the form "A6 to B4," i.e. 
    a letter A..H or a..h and number 1..8 pairing followed by "to," 
    followed by another letter/number pair. 
    Examples: 
    - [parse "quit"] is [Quit] 
    - [parse "  Quit  "] is [Quit]. 
    - [parse "draw"] is [Draw]
    - [parse "   Draw "] is [Draw]
    - [parse "help"] is [Help]
    - [parse "A6 to B4"] is [Move ('A',6,'B',4')]
    - [parse "  b3 to   c7"] is [Move ('B',3,'C',7)]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Invalid] if [str] is not one of the above discussed valid
    input formats *)
val parse : string -> t 



