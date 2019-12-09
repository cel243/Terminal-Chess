(** Display is a printer that handles all view features of the chess game. 
    For example, Display handles all printing of the chess board, printing
    of statements relevant to the players, etc. *)

(** [print_board b] displays to the console all of the pieces on board [b]
    with a checkered background. Rows are preceded by corresponding numbers
    and columns have the representative letter (one of A through H) displayed
    beneath them. *)
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

(** [get_rep_long p] is the full string representation of the piece type [p]. *)
val get_rep_long : Board.piece -> string 

(** [get_color_str c] is "Black" if [c] is [Board.Black], and is "White"
    if [c] is [Board.White] *)
val get_color_str : Board.color -> string 

(** [get_opp_color_str c] is "Black" if [c] is [Board.White], and is "White"
    if [c] is [Board.Black] *)
val get_opp_color_str : Board.color -> string 

(** [capture_message brd c1 i1 c2 i2] prints a message detailing which
    piece has been captured by the previous move, if any.  
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
    "(k) [col] p at (c,i) CAPTURES [col'] p' at (c',i')" 
    Moves that saw a piece (c,i) take a piece (c',i') by en passant are shown:
    "(k) [col] p at (c,i) CAPTURES [col'] p' at (c',i') BY EN PASSANT" 
*)
val print_log : Board.t -> unit

(** [get_input ()] is the next line inputted by the user to stdin, or 
    exits the application if it encounters an EOF (i.e., CTRL-D). *)
val get_input : unit -> string

(** [print_move i ((p,col,c,i), (c',i'), (p',c'',i'') option) b] prints the 
    given move for turn [i]. If [b] is [true], then "Last Move: " is prepended
    to whatever output is determined. The format of the displayed string is 
    the exact same for moves printed via [print_log], and are as follows:
    Let col' be the string representation of the enemy's piece.
    Moves that saw a piece (c,i) move to an empty square (c',i') are shown as:
    "(i) [col] p at (c,i) TO (c',i')"
    Moves that saw a piece (c,i) take a piece (c',i') are shown as: 
    "(i) [col] p at (c,i) CAPTURES [col'] p' at (c',i')" 
    Moves that saw a piece (c,i) take a piece (c',i') by en passant are shown:
    "(i) [col] p at (c,i) CAPTURES [col'] p' at (c'',i'') BY EN PASSANT" 
*)
val print_move : int -> 
  ((Board.piece * Board.color * char * int) * (char * int) 
   * ((Board.piece * char * int) option)) -> bool -> unit