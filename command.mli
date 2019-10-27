(** This module parses player commands in the chess game *)

(** the type of a location on the chess board, where the columns
    are labeled A..H from left to right, and the rows are labelled 
    1..8 from bottom to top *)
type location = char * int 

(** the type of a player command *)
type t = 
  | Quit 
  | Draw 
  | Restart 
  | Move of location*location

(** raised when the command isn't one of the expected forms  *)
exception Invalid 

(** TODO *)
val parse : string -> t 



