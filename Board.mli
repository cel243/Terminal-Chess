(** Representation of the dynamic state of the chess game.

    This module represents the state of the board, including
    where all the pieces are located, what the previous legal moves have 
    been, and which player's turn it is. 

    It contains functions to mutate the state of the board.  *)

(** The abstract type of the value representing the board itself,
    which contains information about where each piece is located. *)
type t 

type piece = Pawn | Rook (*  | .... *)
type color = Black | White 
type game_piece = {p_type : piece; col : color; has_moved : bool }

(** [init_state] is the initial state of the board when a chess
    game is started. *)
val init_state : t 

(** [get_piece_at b c i] is [Some p], where [p] is the chess piece at c,i on 
    board [b], if such a piece exists, where the board is labelled A..H from 
    left to right, and 1..8 from bottom to top. If there is no piece at that 
    location, [get_piece_at c i] is [None]. *)
val get_piece_at : t -> char -> int -> game_piece option 

(** [get_white_pieces b] is the list of all game pieces labelled as
    [White] on board [b], where each item of the list is of the form 
    [(p, c, i)], where [p] is the game piece, [c] is the char location
    of this piece, and [i] is the int location.   *)
val get_white_pieces : t -> (game_piece * char * int) list 

(** [get_black_pieces b] is the list of all game pieces labelled as
    [Black] on board [b], where each item of the list is of the form 
    [(p, c, i)], where [p] is the game piece, [c] is the char location
    of this piece, and [i] is the int location.   *)
val get_black_pieces : t -> (game_piece * char * int) list 

(** [move_piece b c1 i1 c2 i2] is board [b] after the piece at
    [c1, i1] on the board has replaced whatever was previously at
    [c2, i2] on the board , leaving an empty square at [c1, i1]. *)
val move_piece : t -> char -> int -> char -> int -> t 