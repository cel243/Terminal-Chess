
let handle_result b = function
  | Legal -> Board.next_player b
  | Illegal -> print_string "That move is illegal. Please retry.\n"
  | Terminate -> begin
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
  | Move _ as c -> begin
      (Logic.process b c) |> handle_result b
    end
  | exception Command.Invalid -> 
    print_string "Invalid command.\n"
      x
let rec play_chess b = 
  Display.print_board b;
  match (Board.get_current_player b) with
  | Black -> print_string "Black's move.\n";
  | White -> print_string "White's move.\n";
    print_string "> ";

    match read_line () with
    | exception End_of_file -> print_string "Goodbye.\n"; ()
    | str -> parse_input b str; play_chess b

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "Welcome to chess.";
  print_string  "> ";
  Board.init_state |> play_chess

(* Execute the game engine. *)
let () = main ()