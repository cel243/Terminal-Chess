(** A printer that takes in a game and displays the current board visually
    so that players can easily understand what the current state of the game 
    looks like. *)

(** [print_board g] prints a visual representation of the board of 
    game [g] *)
val print_board : Board.t -> unit 