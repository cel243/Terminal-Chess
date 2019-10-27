
type locations = char * int * char * int 
type t = 
  | Quit 
  | Draw 
  | Move of locations
exception Invalid 

let get_command wrd_ls = 
  match wrd_ls with 
  | ["QUIT"] -> Quit 
  | ["DRAW"] -> Draw 
  | [l1; "TO"; l2] when String.length l1 = 2 && String.length l2 = 2 -> 
    Move (String.get l1 0, int_of_char (String.get l1 1),
          String.get l2 0, int_of_char (String.get l2 1) )
  | _ -> raise Invalid  

(* val parse : string -> t  *)
let parse p_in = 
  String.split_on_char ' ' p_in 
  |> List.map (fun s -> String.uppercase_ascii s)
  |> get_command  




