(** Yojson.Basic.from_file f *)
open Yojson.Basic.Util

let ptype_from_string = function 
  | "pawn" -> Board.Pawn 
  | "rook" -> Board.Rook
  | "bishop" -> Board.Bishop 
  | "knight" -> Board.Knight
  | "queen" -> Board.Queen
  | "king" -> Board.King 
  | s -> failwith (" ptype precond violated: "^s) 

let col_from_string = function 
  | "white" -> Board.White 
  | "black" -> Board.Black 
  | s -> failwith ("color precond violated: "^s) 

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

let save_game f_name brd = 
  let brd_arr = Board.board_to_array brd in 
  let json : Yojson.Basic.t  = `Assoc [
      ("current player", 
       `String (Display.get_color_str (Board.get_current_player brd) 
                |> String.lowercase_ascii));
      ("board", form_board brd_arr)] in 
  Yojson.Basic.to_file (f_name^".json") json 

let get_piece p = 
  let pt = p |> member "p_type" |> to_string in 
  if  pt = "none" then None 
  else Some Board.{
      p_type=ptype_from_string pt; 
      col=col_from_string (p |> member "col" |> to_string);
      has_moved= p|> member "has_moved" |> to_bool;
      points= p|> member "points" |> to_int       
    }

let get_board_pieces json = json |> member "board" |> to_list 

let get_board  json = 
  let p_ls = get_board_pieces json in 
  let brd = Array.make_matrix 8 8 None in 
  let counter = ref 0 in 
  for row=0 to 7 do 
    for col = 0 to 7 do 
      brd.(row).(col) <- get_piece (List.nth p_ls (!counter));
      counter := !counter + 1
    done
  done;
  brd  

let get_turn json = col_from_string (json |> member "current player" |> to_string)

let load_game f = 
  let json = Yojson.Basic.from_file f in 
  Board.set_game (get_turn json) (get_board json)


