(** Representation of the dynamic state of the chess game.

    This module represents the state of the board, including
    where all the pieces are located, what the previous legal moves have 
    been, and which player's turn it is. 

    It contains functions to mutate the state of the board.  *)

(** The abstract type of the value representing the chess game,
    which contains information about where each piece is located. *)
type t 

(** the type of a chess piece, where the piece is one of 
    the 6 standard chess peices *)
type piece = Pawn | Rook | Bishop | Knight | Queen | King 

(** the color black or white *)
type color = Black | White 

(** the type of a piece in the chess game, containing a type, 
    a color denoting which player the piece belongs to, 
    and whether the piece has been moved previously.  *)
type game_piece = {p_type : piece; col : color; has_moved : bool }

(** [init_state] is the initial state of the game when a chess
    game is started. *)
val init_state : t 

(** [get_current_player g] is [White] if it is the white player's
    turn to move in game [g] and [Black] otherwise *)
val get_current_player : t -> color 

(** [update_current_player g] updates the color of the current player 
    so that it becomes the opposite color. *)
val next_player : t -> unit 

(** [get_piece_at g c i] is [Some p], where [p] is the chess piece at c,i on
    the board of game [g], if such a piece exists, where the board is 
    labelled A..H from left to right, and 1..8 from bottom to top. 
    If there is no piece at that location, [get_piece_at c i] is [None]. *)
val get_piece_at : t -> char -> int -> game_piece option 

(** [get_white_pieces g] is the list of all game pieces labelled as
    [White] in game [g], where each item of the list is of the form 
    [(p, c, i)], where [p] is the game piece, [c] is the char location
    of this piece, and [i] is the int location.   *)
val get_white_pieces : t -> (game_piece * char * int) list 

(** [get_black_pieces g] is the list of all game pieces labelled as
    [Black] in game [g], where each item of the list is of the form 
    [(p, c, i)], where [p] is the game piece, [c] is the char location
    of this piece, and [i] is the int location.   *)
val get_black_pieces : t -> (game_piece * char * int) list 

(** [move_piece g c1 i1 c2 i2] updates the board of chess game [g] so that 
    [c1, i1] on the board has replaced whatever was previously at
    [c2, i2] on the board , leaving an empty square at [c1, i1]. *)
val move_piece : t -> char -> int -> char -> int -> unit 