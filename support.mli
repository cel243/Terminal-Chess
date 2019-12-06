(** Offers functions that help the player as they play the game, like 
    showing legal moves, which pieces are in danger, suggesting moves,
    etc.   *)

(**  [handle_player_support brd req] handles player request [req] 
     for special support, and is [(locs, b, hyp, b')]. [locs] is 
     always a list of locations that should be highlighted when 
     [b] is printed. [hyp] is true if [req] is a hypothetical command. 
     - If [hyp] is [true], then [b] is a board where the hypothetical move 
       detailed in [req] has been made, and [b'] is a board where 
       the hypothetical move has not been made. 
     - If [hyp] is [false], then both [b] and [b'] are the current board. 

     Will print an error message if the request is not valid, and may 
     print additional information about the results of the
     support request.  
     Will never modify whose turn it is in the state of [brd]. 

     Examples: 
     - if the player requests to see the legal moves of a piece, 
       [handle_player_support brd cmmd] is [(locs, brd, false, brd)], where 
       [locs] is the list of locations that piece can move to. 
     - If the player asks to see which of their pieces will be under 
       attack if they move from C2 to C4, then [handle_player_support brd cmmd] 
       is [(locs, hyp_brd, true, brd)], where [locs] is the locations
       of the pieces that are under attack after the moving from 
       C2 to C4, and [hyp_brd] is a board where C2 has been moved to C4. 
     - if a player asks for information about an empty square, 
       [handle_player_support] will print an appropriate error message. *)
val handle_player_support : 
  Board.t -> Command.request -> (char * int) list * Board.t * bool * Board.t


