(** Handles interactions with JSON files, including saving
    and loading chess games. *)

(** [save_game f_name brd] creates a file named [f_name.json[ that stores
    information about the current game board state in JSON format
    so that the game can be loaded from the file later.  *)
val save_game : string -> Board.t -> unit

(** [load_game f_name] is the chess game represented in the file 
    named [f_name].  *)
val load_game : string -> Board.t