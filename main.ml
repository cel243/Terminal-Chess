open Unix

type gametype = 
  | Exhibition of Board.t 
  | Series

let rec handle_load () = 
  print_string "Please enter the name of the saved game file.\n";
  print_string "> ";
  let f_name =  (Display.get_input ()) |> String.uppercase_ascii in 
  try let brd = FileHandler.load_game (f_name^".json")  in 
    Exhibition brd 
  with _ -> 
    print_string "Invalid file name. Please try again.\n";
    print_string "> ";
    handle_load ()


let rec prompt_gametype () = 
  print_string "Which mode would you like to play?\n";
  print_string "For an exhibiton match, type \"quick\".\n";
  print_string "For a tournament, type \"tournament\".\n";
  print_string "To load a previous game, type \"load\".\n";
  print_string "> ";
  match (Display.get_input ()) |> String.lowercase_ascii with
  | "quick" -> Exhibition (Board.init_state ())
  | "tournament" -> Series
  | "load" -> handle_load () 
  | "exit" -> print_string "Goodbye!"; exit 0
  | _ -> print_string "Invalid mode. Please retry.\n"; prompt_gametype ()

let rec prompt_opponent_type () = 
  print_string "Who would you like to play against?\n";
  print_string "For a human player, type \"local\"\n";
  print_string "For a computer player, type \"cpu\"\n";
  print_string "> ";
  match (Display.get_input ()) |> String.lowercase_ascii with
  | "local" -> Game.Human
  | "cpu" -> Game.CPU
  | _ -> begin
      print_string "Invalid player type. Please retry.\n"; 
      prompt_opponent_type ()
    end

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.erase ANSITerminal.Screen;
  print_string "Welcome to chess.\n";
  let opp = prompt_opponent_type () in
  match (prompt_gametype ()) with 
  | Exhibition brd -> begin
      let _ = Game.play brd opp in ()
    end
  | Series -> begin
      ((Tournament.create opp) |> Tournament.play); ()
    end

(* Execute the game engine. *)
let () = main ()