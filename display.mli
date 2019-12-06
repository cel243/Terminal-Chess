(** A printer that takes in a game and displays the current board visually
    so that players can easily understand what the current state of the game 
    looks like. *)

(** [print_board b] prints all of the pieces in board [b] on a checkered
    background and with pieces color-coded by player. Files are indicated by
    lettering along the bottom; ranks by numbers along the side. *)
val print_board : Board.t -> unit 

(** [p_support_display (locs, b, hyp, b')] does one of two things. 
    If [hyp] is true, it first prints 'hypothetical' board [b]
    as [print_board] would, but with all of the locations in 
    [locs] highlighted. Then it prints [b'] normally. If [hyp]
    is false, it simply prints [b] with the locations in [locs]
    highlighted. *)
val p_support_display : (char * int) list * Board.t * bool * Board.t -> unit

(** [help_menu ()] prints a menu detailing accepted player commands *)
val help_menu : unit -> unit 

(** [print_captured_pieces brd col] prints the pieces player [col]
    has captured.  *)
val print_captured_pieces : Board.t -> Board.color -> unit 

(** [get_rep_long ptype] is the string representation of [ptype] *)
val get_rep_long : Board.piece -> string 

(** [get_color_str col] is the string representation of [col] *)
val get_color_str : Board.color -> string 

(** [get_opp_color_str col] is the string representation of opposite [col] *)
val get_opp_color_str : Board.color -> string 

(** [capture_message brd c1 i1 c2 i2] prints a message detailing which
    piece has been captured by the prvious move, if any.  
    Requires: [c1,i1] to [c2,i2] is a legal move *)
val capture_message : Board.t -> char -> int -> char -> int -> unit

(** [print_log b] prints the move list for board [b].
    Let k be the kth move in the game.
    Let p be the string representation of the mover's piece.
    Let p' be the string representation of the enemy's piece.
    Let col be the string representation of the mover's color.
    Let col be the string representation of the enemy's color.
    Moves that saw a piece (c,i) move to an empty square (c',i') are shown as:
    "(k) [col] p at (c,i) TO (c',i')"
    Moves that saw a piece (c,i) take a piece (c',i') are shown as: 
    "(k) [col] p at (c,i) CAPTURES [col'] p' at (c',i')" *)
val print_log : Board.t -> unit

val get_input : unit -> string

val print_move : int -> 
  ((Board.piece * Board.color * char * int) * (char * int) 
   * ((Board.piece * char * int) option)) -> unit