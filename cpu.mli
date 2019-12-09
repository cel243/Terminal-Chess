(** Cpu hosts all necessary functions for the computer to make intelligent 
    chess moves and theorize about possible chess moves. *)

(** [next_move brd] is the next move (c1,i1,c2,i2) the cpu recommends *)
val next_move : Board.t -> char * int * char * int