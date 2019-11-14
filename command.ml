
type request = 
  | CanCapture of char * int 
  | LegalMoves of char * int 
  | UnderAttack 
  | Attackers of char * int 
  | UnderAttackIF of char * int * char * int 
  | Log
type locations = char * int * char * int 
type t = 
  | Resign 
  | Draw 
  | Help
  | Captured
  | Move of locations
  | PSupport of request 
exception Invalid 

(** [get_command wrd_ls] is the command that [wrd_ls] represents. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_command wrd_ls = 
  match wrd_ls with 
  | ["RESIGN"] -> Resign 
  | ["DRAW"] -> Draw 
  | ["HELP"] -> Help 
  | ["CAPTURED"] -> Captured
  | [loc] when String.length loc = 2 -> 
    PSupport (
      LegalMoves 
        (String.get loc 0, int_of_char (String.get loc 1) - 48))
  | [l1; "TO"; l2] when String.length l1 = 2 && String.length l2 = 2 -> 
    Move (String.get l1 0, int_of_char (String.get l1 1) - 48,
          String.get l2 0, int_of_char (String.get l2 1) - 48 )
  | ["LOG"] -> PSupport (Log)
  | _ -> raise Invalid  


let parse p_in = 
  String.split_on_char ' ' p_in 
  |> List.filter (fun x -> x <> "") 
  |> List.map (fun s -> String.uppercase_ascii s)
  |> get_command  




