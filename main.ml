
let handle_result b = function
  | Logic.Legal -> Board.next_player b
  | Logic.Illegal str -> 
    print_string ("That move is illegal. "^str^" Please retry.\n")
  | Logic.Terminate -> begin
      match (Board.get_current_player b) with
      | Black -> print_string "Checkmate! Black wins!\n"; exit 0
      | White -> print_string "Checkmate! White wins!\n"; exit 0
    end

let parse_input b str = 
  match (Command.parse str) with
  | Quit -> begin
      print_string "Goodbye.\n";
      exit 0
    end
  | Draw -> begin
      print_string "It's a draw!\n";
      exit 0
    end
  | Help -> Display.help_menu ()
  | Move _ as c -> begin
      (Logic.process b c) |> handle_result b
    end
  | exception Command.Invalid -> 
    print_string "Invalid command.\n";
    Display.help_menu ()

let print_move = function
  | Board.Black -> print_string "Black's move.\n"; ()
  | Board.White -> print_string "White's move.\n"; ()

let rec play_chess b = 
  Display.print_board b;
  (Board.get_current_player b) |> print_move;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> print_string "Goodbye.\n"; ()
  | str -> parse_input b str; play_chess b

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "Welcome to chess.\n";
  Board.init_state () |> play_chess

(* Execute the game engine. *)
let () = main ()