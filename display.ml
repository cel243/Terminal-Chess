open ANSITerminal

(** dark = blue, light = cyan *)

let get_rep = function
  | Board.Pawn -> "P"
  | Board.Rook -> "R"
  | Board.Bishop -> "B"
  | Board.Knight -> "N"
  | Board.Queen -> "Q"
  | Board.King -> "K"

let get_background r f = 
  if (f mod 2 = 0 && r mod 2 = 0) || (not (f mod 2 = 0) && (r mod 2 = 0)) then
    ANSITerminal.on_blue
  else
    ANSITerminal.on_cyan

let get_foreground (c : Board.color) = 
  match c with
  | Black -> ANSITerminal.black
  | White -> ANSITerminal.white

let print_rank r b =
  ANSITerminal.print_string [white; on_black] (" "^(string_of_int r)^" ");
  for f = 1 to 8 do
    let bg = get_background r f in
    match (Board.get_piece_at b (char_of_int (64 + f)) r) with
    | None -> ANSITerminal.print_string [bg] "   "
    | Some p -> begin
        let s = get_rep p.p_type in
        ANSITerminal.print_string [(get_foreground p.col); bg] (" "^s^" ")
      end
  done;
  ANSITerminal.print_string [default] "\n";
  ()

let print_board b =
  ANSITerminal.print_string [default] "\n";
  for r = 1 to 8 do
    print_rank r b
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n";