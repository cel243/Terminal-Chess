(** Offers functions that help the player as they play the game, like 
    showing legal moves, which pieces are in danger, suggesting moves,
    etc.   *)

(* 
we can add a command type, PSupport of request, which would call the support
module to handle all of these! 
*)

type request = 
  | CanCapture of char * int 
  | LegalMoves of char * int 
  | UnderAttack 
  | Attackers of char * int 
  | UnderAttackIF of char * int * char * int 

(**  [handle_player_support req] handles player requests for special support, 
     either printing a chess board that visually addressed the request, 
     or printing an error message if the request is not valid. 
     Will not modify whose turn it is.
     Examples: 
     - if the player requests to see all of the pieces that are attacking
       a given location, [handle_player_support] will print a board with
       the potential attackers highlighted. 
     - if a player asks for information about an empty square, 
       [handle_player_support] will print an appropriate error message.  *)
val handle_player_support : Board.t -> request -> unit 


