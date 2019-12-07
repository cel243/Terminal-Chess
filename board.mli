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
type game_piece = 
  {p_type : piece; col : color; has_moved : bool; points : int }

(** [init_state] is the initial state of the game when a chess
    game is started. *)
val init_state : unit -> t 

(** [set_game col brd_arr log w_cap b_cap] is a chess game in which 
    the current player is [col], all the pieces are arranged
    as represented in [brd_arr], all previous moves
    are represented in [log], and white and black have captured
    pieces as indicated by [w_cap] and [b_cap]. 
*)
val set_game : color -> game_piece option array array ->
  ((piece * color * char * int) * (char * int) * (piece * char * int) option)
    list -> (piece * int) list -> (piece * int) list -> t

(** [get_current_player g] is [White] if it is the white player's
    turn to move in game [g] and [Black] otherwise *)
val get_current_player : t -> color 

(** [board_to_array g] is the board in game [g] represented as an 
    array.  *)
val board_to_array : t -> game_piece option array array

(** [log_to_list g] is the log of moves represented as a list.  *)
val log_to_list : t ->
  ((piece * color * char * int) * (char * int) * (piece * char * int) option)
    list

(** [white_cap_to_list g] is an association list representation
    mapping piece type to the number of piece of that type
    white has captured  *)
val white_cap_to_list : t -> (piece * int) list

(** [black_cap_to_list g] is an association list 
    mapping piece types to the number of piece of that type
    black has captured  *)
val black_cap_to_list : t -> (piece * int) list

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
    [c2, i2] on the board , leaving an empty square at [c1, i1].
    if [c1, i1] is a pawn then it is promoted to a Queen *)
val move_piece : t -> char -> int -> char -> int -> unit 

val move_piece_en_passant : t -> char -> int -> char -> int -> char -> int -> unit 

(** [copy_board b] is a copy of [b] *)
val copy_board : t -> t 

(** [get_opp_color col] is [Board.Black] is [col] is [Board.White], and is
    [Board.White] is [col] is [Board.White] *)
val get_opp_color : color -> color

(**  [capture_piece state col piece] updates the the list of captured
     pieces for player [col] with [piece].  *)
val capture_piece : t -> color -> piece -> unit

(**  [get_captured_pieces state col] is the association list of pieces
     captured by player [col] mapped to the number of this particular 
     piece player [col] has captured.   *)
val get_captured_pieces : t -> color -> (piece*int) list 

(** [get_moves state] returns a list of moves that have performed on
    the given Board. Each element of the list is of the form:
    1) ((p1,c1,i1), (Some p2, c2, i2)) if there was a piece at the destination
    2) ((p1,c1,i1), (None, c2, i2)) if there was no piece at the destination 
*)
val get_moves : t -> 
  ((piece * color * char * int) * (char * int) * ((piece * char * int) option))
    list

val get_score : t -> color -> int
val get_score_cpu : t -> color -> int

val get_last_move : t ->
  ((piece * color * char * int) * (char * int) * ((piece * char * int) option))
    option

val get_move_cnt : t -> int