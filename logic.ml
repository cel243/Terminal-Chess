
type res = Legal | Illegal | Terminate 

let is_valid_location c i =
  (let n = int_of_char c in (n >= 97 && n <= 104) || (n >= 65 && n <= 72)) &&
  (i >= 1 && i <= 8) 

let is_curr_players b c i = 
  match (Board.get_piece_at b c i) with
  | None -> false
  | Some {p_type=_; col; has_moved=_} -> col = (Board.get_current_player b)

let is_valid_positionally brd c1 i1 c2 i2 =
  not ((not (is_valid_location c1 i1) || not (is_valid_location c2 i2)) ||
       (not (is_curr_players brd c1 i1) || (is_curr_players brd c2 i2)))

let move brd c1 i1 c2 i2 =
  (Board.move_piece brd c1 i1 c2 i2);
  Legal

let process brd cmmd = 
  match cmmd with 
  | Command.Move (c1,i1,c2,i2) -> begin
      (* if (not (is_valid_location c1 i1) || not (is_valid_location c2 i2)) then
         Illegal
         else if (not (is_curr_players brd c1 i1) || (is_curr_players brd c2 i2)) then
         Illegal *)
      if not (is_valid_positionally brd c1 i1 c2 i2) then
        Illegal
      else
        move brd c1 i1 c2 i2
    end
  | _ -> failwith "impossible"