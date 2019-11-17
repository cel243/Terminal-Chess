
(** Abstract type representing a chess tournament. *)
type t

type player = 
  | PlayerOne
  | PlayerTwo

type outcome = 
  | Win
  | Loss
  | Draw

type score = float

val create : int -> t

val to_score : outcome -> score

val get_games_played : t -> int

val is_tied : t -> bool
val get_winner : t -> player option

val get_score : t -> player -> score
val update : t -> player -> outcome -> t

val get_best_of_cnt : t -> int
val get_player_with_color : t -> Board.color -> player

val get_new_player_color : t -> player -> Board.color

val display_tourny : t -> unit
val display_game : t -> unit
val display_final : t -> unit