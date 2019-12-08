(** [next_move brd] is the next move (c1,i1,c2,i2) the cpu recommends *)
val next_move : Board.t -> char * int * char * int