

(** [get_pieces brd counter] makes a list of all the pieces on the board 
    belonging to the current player of the form (Piece,rank,file,score). *)
val get_pieces : Board.t -> int -> (Board.piece * char * int * int) list

(** [get_moves_piece brd counter c1 i1] is the list of all possible moves 
    of the piece at [c1, i1]. *)
val get_moves_piece : 
  Board.t -> int -> char -> int -> (char * int * char * int) list

(** [get_moves brd lst] is the list of all possible moves for every piece in 
    [lst]. *)
val get_moves : 
  Board.t -> ('a * char * int * 'b) list -> (char * int * char * int) list

(** [get_rand_move brd] is the next move (c1,i1,c2,i2) 
    the cpu chooses at random *)
val get_rand_move : Board.t -> (char*int*char*int)