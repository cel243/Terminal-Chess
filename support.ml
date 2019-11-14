open ANSITerminal 


(* 
FUNCTIONS TO SUPPORT? 
   show player all pieces it can take (and how to take them) 
   show player all legal moves of a piece 
   show current player all pieces under attack 
   show player which pieces are attacking this piece 
   show player top 3 suggested moves 
   show player which pieces will be under attack IF they make a given move

*)

let rec check_rows c' ints brd c i = 
  match ints with 
  | [] -> [] 
  | i'::t ->  
    let (b, _) = Logic.is_legal brd c i c' i' in
    if b
    then (c',i')::(check_rows c' t brd c i)  
    else check_rows c' t brd c i

let rec check_cols chars ints brd c i sofar = 
  match chars with 
  | [] -> sofar 
  | c'::t -> check_cols t ints brd c i ((check_rows c' ints brd c i)@sofar)

let get_legal_squares brd c i  = 
  let chars = ['A';'B';'C';'D';'E';'F';'G';'H'] in 
  let ints =  [1;2;3;4;5;6;7;8] in 
  check_cols chars ints brd c i  [] 


let legal_moves brd c i = 
  if not (Logic.is_valid_location c i ) then 
    print_string [red] "\nYou have not enetered a valid location.\n"
  else 
    match Board.get_piece_at brd c i with 
    | None -> print_string [red] "\nThere is no piece on this square.\n"
    | Some {col} -> 
      if Board.get_current_player brd <> col then 
        print_string [red] "\nThat is not your piece.\n"
      else 
        Display.print_highlighted_brd brd ((c,i)::(get_legal_squares brd c i))  

let handle_player_support brd = function 
  | Command.CanCapture (c,i) -> failwith "unimplemented"
  | Command.LegalMoves (c,i) -> legal_moves brd c i
  | Command.UnderAttack  -> failwith "unimplemented"
  | Command.Attackers (c,i) -> failwith "unimplemented"
  | Command.UnderAttackIF (c1,i1,c2,i2) -> failwith "unimplemented"
  | Command.Log -> Display.print_log brd