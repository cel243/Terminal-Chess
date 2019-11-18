
type gametype = 
  | Exhibition
  | Series

let rec prompt_gametype () = 
  print_string "Which mode would you like to play?\n";
  print_string "For an exhibiton match, type \"quick\".\n";
  print_string "For a tournament, type \"tournament\".\n";
  print_string "> ";
  match (Display.get_input ()) |> String.lowercase_ascii with
  | "quick" -> Exhibition
  | "tournament" -> Series
  | _ -> print_string "Invalid mode. Please retry.\n"; prompt_gametype ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.erase ANSITerminal.Screen;
  print_string "Welcome to chess.\n";
  match (prompt_gametype ()) with 
  | Exhibition -> begin
      let _ = Game.play () in ()
    end
  | Series -> begin
      ((Tournament.create ()) |> Tournament.play); ()
    end

(* Execute the game engine. *)
let () = main ()