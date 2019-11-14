open ANSITerminal

(** dark = blue, light = cyan *)

(** [get_rep p] returns the one character representation of game piece [p].
    Pawn: P
    Rook: R
    Bishop: B
    Knight: N
    Queen: Q
    King: K *)
let get_rep col p = match col, p with
  | Board.White, Board.Pawn -> "\u{2659}"
  | Board.White, Board.Rook -> "\u{2656}"
  | Board.White, Board.Bishop -> "\u{2657}"
  | Board.White, Board.Knight -> "\u{2658}"
  | Board.White, Board.Queen -> "\u{2655}"
  | Board.White, Board.King -> "\u{2654}"
  | Board.Black, Board.Pawn -> "\u{265F}"
  | Board.Black, Board.Rook -> "\u{265C}"
  | Board.Black, Board.Bishop -> "\u{265D}"
  | Board.Black, Board.Knight -> "\u{265E}"
  | Board.Black, Board.Queen -> "\u{265B}"
  | Board.Black, Board.King -> "\u{265A}"

let get_rep_long = function
  | Board.Pawn -> "Pawn"
  | Board.Rook -> "Rook"
  | Board.Bishop -> "Bishop"
  | Board.Knight -> "Knight"
  | Board.Queen -> "Queen"
  | Board.King -> "King"

(** [get_background r f] is [ASNITerminal.on_blue] if either the file [f]
    or rank [r] is even, but not both; otherwise, is [ANSITerminal.on_cyan]. *)
let get_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) then
    ANSITerminal.on_blue
  else
    ANSITerminal.on_cyan

(** [get_highlighted_background r f] is [ASNITerminal.on_yellow] if either the file [f]
    or rank [r] is even, but not both; otherwise, is [ANSITerminal.on_red]. *)
let get_highlighted_background r f = 
  if (f mod 2 = 0 && not (r mod 2 = 0)) || (not (f mod 2 = 0) && (r mod 2 = 0)) then
    ANSITerminal.on_red
  else
    ANSITerminal.on_yellow

(** [get_foreground c] is [ANSITerminal.black] if [c] is [Black], and
    is [ANSITerminal.white] if [c] is [White]. This function serves
    as a translator between the UI colors and internal logic designators. *)
let get_foreground (c : Board.color) = 
  match c with
  | Black -> ANSITerminal.black
  | White -> ANSITerminal.white

(** [print_rank r b] prints the rank (row) [r] given the pieces in board [b].
    The printed row is preceded by its number on a black background and is
    fololowed by a new line. Pieces are displayed via [get_rep] and empty
    squares are shown as spaces. *)
let print_rank r b =
  ANSITerminal.print_string [white; on_black] (" "^(string_of_int r)^" ");
  for f = 1 to 8 do
    let bg = get_background r f in
    match (Board.get_piece_at b (char_of_int (64 + f)) r) with
    | None -> ANSITerminal.print_string [bg] "   "
    | Some p -> begin
        let s = get_rep p.col p.p_type in
        let chck_cnd = (Logic.king_in_check b) && (p.col = Board.get_current_player b) in
        match p.p_type with 
        | Board.King when chck_cnd -> begin
            ANSITerminal.print_string [(get_foreground p.col); on_red] (" "^s^" ")
          end
        | _ -> begin
            ANSITerminal.print_string [(get_foreground p.col); bg] (" "^s^" ")
          end
      end
  done;
  ANSITerminal.print_string [default] "\n";
  ()

let print_board b =
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank r b
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"

let get_color_str = function
  | Board.Black -> "Black"
  | Board.White -> "White"

let get_opp_color_str = function
  | Board.White -> "Black"
  | Board.Black -> "White"

let print_log b = 
  ANSITerminal.print_string [default] "Move Log:\n";
  let rec print_all_moves i = function
    | [] -> ANSITerminal.print_string [default] "\n"
    | ((p1,col,c1,i1),(None, c2, i2)):: t -> begin
        let cstr = get_color_str col in
        let rep = get_rep_long p1 in
        let i1s = string_of_int i1 in
        let i2s = string_of_int i2 in
        let c1s = Char.escaped c1 in
        let c2s = Char.escaped c2 in
        let is = string_of_int i in 
        let s =
          "("^is^") ["^cstr^"] "^rep^" at ("^c1s^","^i1s
          ^") TO ("^c2s^","^i2s^")"
        in
        ANSITerminal.print_string [default] (s^"\n");
        print_all_moves (i+1) t
      end
    | ((p1,col,c1,i1),(Some p2, c2, i2)):: t -> begin
        let cstr = get_color_str col in
        let cstr2 = get_opp_color_str col in
        let rep1 = get_rep_long p1 in
        let rep2 = get_rep_long p2 in
        let i1s = string_of_int i1 in
        let i2s = string_of_int i2 in
        let c1s = Char.escaped c1 in
        let c2s = Char.escaped c2 in
        let is = string_of_int i in 
        let s = "("^is^") ["^cstr^"] "^rep1^" at ("^c1s^","^i1s^") CAPTURES "^
                "["^cstr2^"] "^rep2^" at ("^c2s^","^i2s^")"
        in
        ANSITerminal.print_string [default] (s^"\n");
        print_all_moves (i+1) t
      end
  in print_all_moves 0 (List.rev (Board.get_moves b))

(** [print_rank_highlighted r b locs col] 
    prints the rank (row) [r] given the pieces in board [b].
    The printed row is preceded by its number on a black background and is
    fololowed by a new line. Pieces are displayed via [get_rep] and empty
    squares are shown as spaces. The backgrounds of locations in 
    [locs] are highlighted with [col]. *)
let print_rank_highlighted r b locs =
  ANSITerminal.print_string [white; on_black] (" "^(string_of_int r)^" ");
  for f = 1 to 8 do
    let ch = char_of_int (f+64) in 
    let bg = (if List.mem (ch, r) locs 
              then get_highlighted_background r f 
              else get_background r f ) in
    match (Board.get_piece_at b (char_of_int (64 + f)) r) with
    | None -> ANSITerminal.print_string [bg] "   "
    | Some p -> begin
        let s = get_rep p.col p.p_type in
        ANSITerminal.print_string [(get_foreground p.col); bg] (" "^s^" ")
      end
  done;
  ANSITerminal.print_string [default] "\n";
  ()

let print_highlighted_brd b locs = 
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank_highlighted r b locs
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"


let help_menu () = 
  ANSITerminal.print_string [red] "\n-----------HELP MENU----------\n";
  ANSITerminal.print_string [red] 
    ("CN to C'N' --> if C and C' are characters in A..H and N and N' are"
     ^" integers in 1..8, then move the piece at CN to C'N', if legal.\n");
  ANSITerminal.print_string [red] "Resign --> forfeit the game\n";
  ANSITerminal.print_string [red] 
    "Draw --> declare a draw between the players \n";
  ANSITerminal.print_string [red] "Help --> display the help menu \n";
  ANSITerminal.print_string [red] 
    "Captured --> display the current player's captured pieces \n";
  ANSITerminal.print_string [red] 
    "CN --> highlights legal moves of the piece at CN.\n";
  ANSITerminal.print_string [red] 
    "LOG --> prints all of the moves performed this game.\n"

let rec print_piece_list = function 
  | [] -> () 
  | (p, n)::t -> 
    ANSITerminal.print_string [red] 
      ((get_rep_long p)^": "^(string_of_int n)^"\n"); 
    print_piece_list t

let print_captured_pieces brd = function 
  | Board.White -> 
    ANSITerminal.print_string 
      [red] "\nWHITE'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd White)  
  | Board.Black -> 
    ANSITerminal.print_string 
      [red] "\nBLACK'S CAPTURED PIECES: \n"; 
    print_piece_list (Board.get_captured_pieces brd Black) 

let capture_message brd c1 i1 c2 i2 = 
  match Board.get_piece_at brd c1 i1, Board.get_piece_at brd c2 i2 with 
  | _, None -> () 
  | Some {p_type=p1;col}, Some {p_type=p2} -> 
    ANSITerminal.print_string 
      [red] 
      ("\n"^(get_rep_long p1)^" takes "^(get_rep_long p2)^"!\n")
  | _ , _ -> failwith "precond violated"