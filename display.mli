(** A printer that takes in a game and displays the current board visually
    so that players can easily understand what the current state of the game 
    looks like. *)

(** [print_board g] prints a visual representation of the board of 
    game [g] *)
val print_board : Board.t -> unit 

(** [help_menu ()] prints a menu detailing accepted player commands *)
val help_menu : unit -> unit 

(** [print_captured_pieces brd col] prints the pieces player [col]
    has captured.  *)
val print_captured_pieces : Board.t -> Board.color -> unit 

(** [get_rep_long ptype] is the string representation of [ptype] *)
val get_rep_long : Board.piece -> string 

(** [capture_message brd c1 i1 c2 i2] prints a message detailing which
    piece has been captured by the prvious move, if any.  
    Requires: [c1,i1] to [c2,i2] is a egal move ]*)
val capture_message : Board.t -> char -> int -> char -> int -> unit