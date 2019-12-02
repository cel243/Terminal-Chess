(** Yojson.Basic.from_file f *)
open Yojson.Basic.Util

(** [ptype_from_string s] is the chess piece type that [s] represents. 
    Requires: [s] is the lowercase full name of one of the six valid 
    chess piece types. *)
let ptype_from_string = function 
  | "pawn" -> Board.Pawn 
  | "rook" -> Board.Rook
  | "bishop" -> Board.Bishop 
  | "knight" -> Board.Knight
  | "queen" -> Board.Queen
  | "king" -> Board.King 
  | s -> failwith (" ptype precond violated: "^s) 

(** [col_from_string s] is the color that [s] represents.
    Requires: [s] is either ["white"] or ["black"] *)
let col_from_string = function 
  | "white" -> Board.White 
  | "black" -> Board.Black 
  | s -> failwith ("color precond violated: "^s) 

(** [form_piece p] is a JSON representation of [p] *)
let form_piece ( p : Board.game_piece ) : Yojson.Basic.t  = 
  `Assoc [("p_type", 
           `String (Display.get_rep_long p.p_type |> String.lowercase_ascii));
          ("col", 
           `String (Display.get_color_str p.col |> String.lowercase_ascii));
          ("has_moved", `Bool p.has_moved);
          ("points", `Int p.points)]

(**  [form_board brd_arr] is a JSON representation of the chess 
     board [brd_arr].  *)
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

(** [form_log_entry e] is a JSON representation of the individual
    entry in the move log [e].  *)
let form_log_entry ((p1, c, c1, i1), (c2, i2), p_op) : Yojson.Basic.t = 
  let base_list = 
    [("p_type", 
      `String (Display.get_rep_long p1 |> String.lowercase_ascii));
     ("col", 
      `String (Display.get_color_str c |> String.lowercase_ascii));
     ("starting char", `String (Char.escaped c1));
     ("starting int", `Int i1);
     ("ending char", `String (Char.escaped c2));
     ("ending int", `Int i2)] in 
  let cap_piece = (
    match p_op with 
    | None -> [("captured piece", `String "none")]
    | Some (p2, c3, i3) -> 
      [("captured piece", 
        `String (Display.get_rep_long p1 |> String.lowercase_ascii));
       ("captured char", `String (Char.escaped c3));
       ("captured int", `Int i2)] ) in 
  `Assoc (base_list@cap_piece)

(**  [form_log log_ls] is a JSON representation of the log of moves
     [log_ls]. *)
let form_log log_ls : Yojson.Basic.t = 
  let l_ls = ref [] in 
  List.iter 
    (fun e -> l_ls := (form_log_entry e)::(!l_ls))
    log_ls; 
  `List (!l_ls)

let save_game f_name brd = 
  let brd_arr = Board.board_to_array brd in 
  let log_ls = Board.log_to_list brd in 
  let json : Yojson.Basic.t  = `Assoc [
      ("current player", 
       `String (Display.get_color_str (Board.get_current_player brd) 
                |> String.lowercase_ascii));
      ("board", form_board brd_arr);
      ("log", form_log log_ls)] in 
  Yojson.Basic.to_file (f_name^".json") json 

(** [get_piece p] is the optional game piece that is represented 
    by JSON object [p].  *)
let get_piece p = 
  let pt = p |> member "p_type" |> to_string in 
  if  pt = "none" then None 
  else Some Board.{
      p_type=ptype_from_string pt; 
      col=col_from_string (p |> member "col" |> to_string);
      has_moved= p|> member "has_moved" |> to_bool;
      points= p|> member "points" |> to_int       
    }

(** [get_board json] is the chess board array represented in [json].  *)
let get_board json = 
  let p_ls = json |> member "board" |> to_list  in 
  let brd = Array.make_matrix 8 8 None in 
  let counter = ref 0 in 
  for row=0 to 7 do 
    for col = 0 to 7 do 
      brd.(row).(col) <- get_piece (List.nth p_ls (!counter));
      counter := !counter + 1
    done
  done;
  brd  

(** [get_piece p] is the individual move log entry that is represented 
    by JSON object [e].  *)
let get_log_entry e = 
  let cap_piece = (
    match (e |> member "captured piece" |> to_string ) with 
    | "none" -> None 
    | s -> Some (
        ptype_from_string s,
        String.get (e |> member "captured char" |> to_string) 0, 
        (e |> member "captured int" |> to_int)
      )
  ) in 
  ((ptype_from_string (e |> member "p_type" |> to_string),
    col_from_string (e |> member "col" |> to_string),
    String.get (e |> member "starting char" |> to_string) 0, 
    (e |> member "starting int" |> to_int)),
   (String.get (e |> member "ending char" |> to_string) 0, 
    (e |> member "ending int" |> to_int)),
   cap_piece
  )

(** [get_log json] is the log list represented in [json]. *)
let get_log json = 
  let l_ls = json |> member "log" |> to_list in 
  let log = ref [] in 
  List.iter (fun e -> log := (get_log_entry e)::(!log)) l_ls;
  !log


(** [get_turn json] is the current player's turn in the game
    stored in [json].  *)
let get_turn json = 
  col_from_string (json |> member "current player" |> to_string)

let load_game f = 
  let json = Yojson.Basic.from_file f in 
  Board.set_game (get_turn json) (get_board json) (get_log json)


