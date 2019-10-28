

(** [step curr dest] is the next value of [curr] in the stepwise
    sweep. *)
let step curr dest = 
  if curr < dest then curr + 1
  else if curr > dest then curr - 1
  else curr 

(** [stepwise brd curr_c curr_i dest_c dest_i] is [true] if 
    there is a chess piece in the chess piece's path from its current
    location to its target destination, not including the target 
    destination. *)
let rec stepwise brd curr_c curr_i dest_c dest_i = 
  if curr_c = dest_c && curr_i = dest_i then false 
  else 
    match Board.get_piece_at brd (char_of_int curr_c) curr_i with
    | None -> stepwise brd (step curr_c dest_c) (step curr_i dest_i) 
                dest_c dest_i 
    | _ -> true 

(** [is_blocked brd c1 i1 c2 i2] is [true] if there is a chess piece in
    between where this piece currently is (c1, i1) and where it is
    trying to move (c2, i2), not including the start and end locations. 
    Otherwise it is [false]. If the piece at (c1, i1) is a knight, then 
    [is_blocked] is always [false]. 
    Requires: there is a piece of the appropriate color at (c1, i1), and 
    neither (c1, i1) nor (c2, i2) is out of bounds.  *)
let is_blocked brd st_c st_i dest_c dest_i = 
  match Board.get_piece_at brd st_c st_i with 
  | None -> failwith "precondition violated" 
  | Some {p_type=Knight} -> false 
  | _ -> stepwise brd 
           (int_of_char st_c + 1) (st_i + 1) (int_of_char dest_c) dest_i


(** [is_legal brd c1 c2 c2 i2] is [true] if the current player 
    moving the piece at [c1, i1] to [c2, i2] is a legal move 
    given the current state of the game.  *)
let is_legal brd c1 i1 c2 i2 =  
  (* all legality tests go here! *)
  not (is_blocked brd c1 i1 c2 i2) 
(* && ...... *)

type res = Legal | Illegal | Terminate 

let process brd cmmd = 
  match cmmd with 
  | Command.Move (c1,i1,c2,i2) -> 
    if is_legal brd c1 i1 c2 i2  
    then (Board.move_piece brd c1 i1 c2 i2; Legal )
    else Illegal 
  | _ -> failwith "impossible"