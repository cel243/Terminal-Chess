(** Yojson.Basic.from_file f *)
open Yojson.Basic.Util

let ptype_from_string = function 
  | "pawn" -> Board.Pawn 
  | "rook" -> Board.Rook
  | "bishop" -> Board.Bishop 
  | "knight" -> Board.Knight
  | "queen" -> Board.Queen
  | "king" -> Board.King 
  | _ -> failwith "precond violated" 

let col_from_string = function 
  | "white" -> Board.White 
  | "black" -> Board.Black 
  | _ -> failwith "precond violated" 

let form_piece ( p : Board.game_piece ) : Yojson.Basic.t  = 
  `Assoc [("p_type", 
           `String (Display.get_rep_long p.p_type |> String.lowercase_ascii));
          ("col", 
           `String (Display.get_color_str p.col |> String.lowercase_ascii));
          ("has_moved", `Bool p.has_moved);
          ("points", `Int p.points)]

let form_board brd_arr : Yojson.Basic.t  = 
  let p_ls = ref [] in 
  for row=7 downto 0 do 
    for col=7 downto 0 do 
      let piece = (
        match brd_arr.(row).(col) with 
        | None -> `Assoc [("p_type", `String "none")]
        | Some p -> form_piece p
      ) in 
      p_ls := piece :: (!p_ls) 
    done 
  done;
  `List (!p_ls) 

let form_save brd_arr curr_player = 
  let json : Yojson.Basic.t  = `Assoc [
      ("current player", `String (Display.get_color_str curr_player));
      ("board", form_board brd_arr)] in 
  Yojson.Basic.to_file "test.json" json 

let get_piece p = 
  let pt = p |> member "p_type" |> to_string in 
  if  pt = "none" then None 
  else Some Board.{
      p_type=ptype_from_string pt; 
      col=col_from_string (p |> member "col" |> to_string);
      has_moved= p|> member "has_moved" |> to_bool;
      points= p|> member "points" |> to_int       
    }

let rec iter_pieces brd row col pieces = 
  match pieces with 
  | [] -> () 
  | p::t -> 
    brd.(row).(col) <- get_piece p;
    iter_pieces brd ((row+1) mod 8) (col+1) t  

let get_board  p_ls = 
  let brd = Array.make_matrix 8 8 None in 
  iter_pieces brd 0 0 p_ls;
  brd 

let get_turn json = col_from_string (json |> member "current player" |> to_string)
