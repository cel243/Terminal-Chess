(** FileHandler handles interactions with JSON files, including saving
    and loading chess games. *)

(** [save_game f_name brd] creates a file named [f_name.json] that stores
    information about the current game board state in JSON format
    so that the game can be loaded from the file later. The file
    will be saved as [f_name.json].   *)
val save_game : string -> Board.t -> unit

(** [load_game f_name] is the chess game represented in the file 
    named [f_name]. 
    Requires: [f_name] is of the form ["NAME.json"], where ["NAME.json"] is 
    a valid JSON file in the current directory.  *)
val load_game : string -> Board.t