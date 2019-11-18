
(** Abstract type representing a chess tournament. 
    Contains information about best-of series, player scores,
    and player color. *)
type t

(** Values representing two players  *)
type player = 
  | PlayerOne
  | PlayerTwo

(** the current points accumulated by a player  *)
type score = float

val create : unit -> t
val to_score : Game.outcome -> score
val get_games_played : t -> int
val is_tied : t -> bool
val get_winner : t -> player option
val get_score : t -> player -> score
val update : t -> Game.outcome option -> t
val get_best_of_cnt : t -> int
val get_player_with_color : t -> Board.color -> player
val get_new_player_color : t -> player -> Board.color
val display_tourny : t -> unit
val display_game : t -> unit
val display_final : t -> unit
val play : t -> unit