open ANSITerminal

(** dark = blue, light = cyan *)

(** [get_rep p] returns the one character representation of game piece [p].
    Pawn: P
    Rook: R
    Bishop: B
    Knight: N
    Queen: Q
    King: K *)
let get_rep = function
  | Board.Pawn -> "P"
  | Board.Rook -> "R"
  | Board.Bishop -> "B"
  | Board.Knight -> "N"
  | Board.Queen -> "Q"
  | Board.King -> "K"

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
        let s = get_rep p.p_type in
        ANSITerminal.print_string [(get_foreground p.col); bg] (" "^s^" ")
      end
  done;
  ANSITerminal.print_string [default] "\n";
  ()

(** [print_board b] prints all of the pieces in board [b] on a checkered
    background and with pieces color-coded by player. Files are indicated by
    lettering along the bottom; ranks by numbers along the side. *)
let print_board b =
  ANSITerminal.print_string [default] "\n";
  for r = 8 downto 1 do
    print_rank r b
  done;
  ANSITerminal.print_string [white; on_black] "    A  B  C  D  E  F  G  H ";
  ANSITerminal.print_string [default] "\n"

let help_menu () = 
  ANSITerminal.print_string [red] "\n-----------HELP MENU----------\n";
  ANSITerminal.print_string [red] "Quit --> exit the game\n";
  ANSITerminal.print_string [red] 
    "Draw --> declare a draw between the players \n";
  ANSITerminal.print_string [red] "Help --> display the help menu \n";
  ANSITerminal.print_string [red] 
    "Captured --> display the current player's captured pieces \n";
  ANSITerminal.print_string [red] 
    ("CN to C'N' --> if C and C' are characters in A..H and N and N' are"
     ^" integers in 1..8, then move the piece at CN to C'N', if legal.\n")

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