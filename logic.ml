
type res = Legal | Illegal | Terminate 


let process brd cmmd = 
  match cmmd with 
  | Command.Move (c1,i1,c2,i2) -> Board.move_piece brd c1 i1 c2 i2
  | _ -> failwith "impossible"