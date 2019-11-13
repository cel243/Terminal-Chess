(** A printer that takes in a game and displays the current board visually
    so that players can easily understand what the current state of the game 
    looks like. *)

(** [print_board b] prints all of the pieces in board [b] on a checkered
    background and with pieces color-coded by player. Files are indicated by
    lettering along the bottom; ranks by numbers along the side. *)
val print_board : Board.t -> unit 

(** [print_highlighted_brd b locs col] prints the same representation of
    the chess board [b] that [print_board] does, but with all of the 
    locations indicated in [loc] highlighted in [col]. *)
val print_highlighted_brd :  Board.t -> (char * int) list -> unit


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