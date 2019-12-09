(** 
   Representation of a series of chess games that track player scores,
   player colors, and the resulting outcome.

   This module provides a type that maintains all of the above information,
   with appropriate methods to modify an instance and extract the desired
   details.
*)

(** Abstract type representing a chess tournament. 
    Contains information about best-of series, player scores,
    and player color. *)
type t

(** Values representing two players *)
type player = 
  | PlayerOne
  | PlayerTwo

(** The current amount of points accumulated by a player  *)
type score = float

(** [create opp] is a newly initialized Tournament.t instance that sees a 
    human Player 1 face off against second player that is either also human
    or the computer, depending on the value of [opp]. *)
val create : Game.opponent -> t

(** [to_score o] is 1.0 if the [o] is a [Win] and is 0.5 if [o] is a [Draw]. *)
val to_score : Game.outcome -> score

(** [get_games_played t] is the number of completed games for tournament [t]. *)
val get_games_played : t -> int

(** [is_tied t] is [true] if Player 1 and Player 2's scores are equal for 
    tournament [t]. *)
val is_tied : t -> bool

(** [get_winner t] is [Some PlayerOne] if Player 1's score is greater than
    Player 2's, or [Some PlayerTwo] if Player 2's score is greater than Player
    1's. If their scores are equal, then this is [None]. *)
val get_winner : t -> player option

(** [get_score t p] is the current score in tournament [t] for player [p]. *)
val get_score : t -> player -> score

(** [update t o] advances the tournament [t] forward one game that resulted
    in the outcome [o], and returns a tournament [t'] with adjusted scores,
    player colors, and other information. *)
val update : t -> Game.outcome option -> t

(** [get_best_of_cnt t] returns the number of games that need to be completed
    in order for this the tournament [t] to be considered finished. *)
val get_best_of_cnt : t -> int

(** [get_player_with_color t col] returns [PlayerOne] if Player 1 currently
    is controlling pieces with color [col], and returns [PlayerTwo] otherwise *)
val get_player_with_color : t -> Board.color -> player

(** [get_new_player_color t p] returns [White] if the player [p] used [Black]
    in the last completed game of the tournament [t], and returns [Black] if
    they instead used [White]. *)
val get_new_player_color : t -> player -> Board.color

(** [display_tourny t] prints the results thus far for tournament [t]. *)
val display_tourny : t -> unit

(** [display_game t] prints the game number and color assignments for the next
    game in tournament [t]. *)
val display_game : t -> unit

(** [display_final t] prints the results for tournament [t] and a winning
    message containg the overall winner of the tournament. *)
val display_final : t -> unit

(** [play t b] advances the tournament [t] to the end, displaying game
    information and playing each game along the way; at the very end, the
    tournament results are displayed and the winner is declared. *)
val play : t -> bool -> unit