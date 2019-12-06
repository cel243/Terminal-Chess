
val get_pieces : Board.t -> int -> (Board.piece * char * int * int) list

val get_moves_piece : Board.t -> int -> char -> int -> (char * int * char * int) list

val get_moves : Board.t -> ('a * char * int * 'b) list -> (char * int * char * int) list

val get_rand_move : Board.t -> (char*int*char*int)