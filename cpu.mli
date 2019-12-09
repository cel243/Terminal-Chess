(** Cpu hosts all necessary functions for the computer to play intelligent 
    chess moves and theorize about possible chess moves. *)

(** [create_board brd c1 i1 c2 i2] is the fake board where the hypothetical 
    move [c1,i1] to [c2,i2] was made *)
val create_board : Board.t -> char -> int -> char -> int -> Board.t

(** [get_boards brd lst] is the list of [((c1,i1,c2,i2),brd)]
    for each move in [lst], 
    where [brd] is the fake board associated with the hypothetical 
    move [c1,i1] to [c2,i2] *)
val get_boards : 
  Board.t -> (char * int * char * int) list -> 
  ((char * int * char * int) * Board.t) list

(** [largest_score score move lst] 
    is the [move] with the largest [score] from [lst] *)
val largest_score : 'a -> 'b -> ('a * 'b) list -> 'b

(** [next_move brd] is the next move (c1,i1,c2,i2) the cpu recommends *)
val next_move : Board.t -> char * int * char * int