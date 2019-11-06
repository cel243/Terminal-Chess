
type locations = char * int * char * int 
type t = 
  | Quit 
  | Draw 
  | Help
  | Captured
  | Move of locations
exception Invalid 

(** [get_command wrd_ls] is the command that [wrd_ls] represents. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_command wrd_ls = 
  match wrd_ls with 
  | ["QUIT"] -> Quit 
  | ["DRAW"] -> Draw 
  | ["HELP"] -> Help 
  | ["CAPTURED"] -> Captured
  | [l1; "TO"; l2] when String.length l1 = 2 && String.length l2 = 2 -> 
    Move (String.get l1 0, int_of_char (String.get l1 1) - 48,
          String.get l2 0, int_of_char (String.get l2 1) - 48 )
  | _ -> raise Invalid  


let parse p_in = 
  String.split_on_char ' ' p_in 
  |> List.filter (fun x -> x <> "") 
  |> List.map (fun s -> String.uppercase_ascii s)
  |> get_command  




