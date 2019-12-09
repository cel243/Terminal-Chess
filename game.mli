(** Game provides a function and types for managing the game flow of
    a single Chess game between two humans or a human and the computer. *)

(** A result of finished chess game that affects a player's score. *)
type outcome = 
  | Win of Board.color
  | Draw

(** The kind of being that acts as the second player versus (human) player 1 *)
type opponent =
  | Human
  | CPU

(** [play b opp is_person] is [Some Win col] if the player with color [col] won the
    game initiated with the layout described by board [b]; it can also be
    [Some Draw] if the result of the game was a draw; and is [None] otherwise *)
val play : Board.t -> opponent -> bool -> outcome option