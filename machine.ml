let rec get_index_at int = function
  | [] -> failwith "index error"
  | h :: t -> if int = 0 then h else get_index_at (int-1) t

let rec get_pieces brd int = 
  if 
    int > -1 
  then
    let c = char_of_int((int/8)+65) in
    let i = (int mod 8)+1 in
    match Board.get_piece_at brd c i with
    | None -> get_pieces brd (int-1)
    | Some {p_type = p; col = col; has_moved; points = pnt} -> 
      if 
        col = Board.get_current_player brd
      then
        (p, c, i, pnt) :: get_pieces brd (int-1)
      else 
        get_pieces brd (int-1)
  else
    []

let rec get_moves_piece brd int c i =
  if 
    int > -1 
  then
    let c1 = char_of_int((int/8)+65) in
    let i1 = (int mod 8)+1 in
    let legal, _ = Logic.is_legal brd c i c1 i1 in
    if legal 
    then (c, i, c1, i1) :: get_moves_piece brd (int-1) c i
    else get_moves_piece brd (int-1) c i
  else
    [] 

let rec get_moves brd = function
  | [] -> []
  | (p, c, i, pnt) :: t -> 
    (get_moves_piece brd (63) c i) @ (get_moves brd t)

let get_rand_move brd = 
  let pieces = get_pieces brd 63 in
  let moves = get_moves brd pieces in
  let rand1 = (int_of_float (Unix.time())) mod 1000000000 in
  let rand = ((Random.int rand1) mod (List.length moves)) in
  get_index_at rand moves


let get_next_move brd =
  MoveTree.next_move brd