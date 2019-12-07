
type locations = char * int * char * int 
type request = 
  | LegalMoves of char * int 
  | LegalMovesIF of char * int * char * int * char * int 
  | UnderAttack 
  | UnderAttackIF of locations
  | CanAttack
  | CanAttackIF of locations
  | Attackers of char * int 
  | AttackersIF of char * int * char * int * char * int 
  | Suggest 
  | SuggestIF of locations
type t = 
  | Resign 
  | Draw 
  | Help
  | Captured
  | Move of locations
  | PSupport of request 
  | Log 
  | Save of string 
exception Invalid 


(** [get_hypothetical_cmmd wrd_ls] is the command that [wrd_ls] represents,
    where [wrd_ls] represents one of the [IF PSupport] commands.
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_hypothetical_cmmd wrd_ls = 
  match wrd_ls with 
  | [loc;"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 
         && String.length loc = 2 -> 
    PSupport 
      (LegalMovesIF (String.get loc 0, int_of_char (String.get loc 1) - 48,
                     String.get l1 0, int_of_char (String.get l1 1) - 48,
                     String.get l2 0, int_of_char (String.get l2 1) - 48 ))
  | ["ATTACKERS";loc;"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 
         && String.length loc = 2 -> 
    PSupport 
      (AttackersIF (String.get loc 0, int_of_char (String.get loc 1) - 48,
                    String.get l1 0, int_of_char (String.get l1 1) - 48,
                    String.get l2 0, int_of_char (String.get l2 1) - 48 ))
  | ["UNDER";"ATTACK";"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport 
      (UnderAttackIF (String.get l1 0, int_of_char (String.get l1 1) - 48,
                      String.get l2 0, int_of_char (String.get l2 1) - 48 ))
  | ["CAN";"ATTACK";"IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport 
      (CanAttackIF (String.get l1 0, int_of_char (String.get l1 1) - 48,
                    String.get l2 0, int_of_char (String.get l2 1) - 48 ))
  | ["SUGGEST"; "IF"; l1; "TO"; l2] 
    when String.length l1 = 2 && String.length l2 = 2 -> 
    PSupport 
      (SuggestIF (String.get l1 0, int_of_char (String.get l1 1) - 48,
                  String.get l2 0, int_of_char (String.get l2 1) - 48 ))
  | _ -> raise Invalid  

(** [get_psupport_command wrd_ls] is the command that [wrd_ls] represents,
    where [wrd_ls] wither represents a Player Support command type. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_psupport_cmmd wrd_ls = 
  match wrd_ls with 
  | [loc] when String.length loc = 2 -> 
    PSupport (
      LegalMoves 
        (String.get loc 0, int_of_char (String.get loc 1) - 48))
  | ["ATTACKERS"; loc] when String.length loc = 2 -> 
    PSupport (
      Attackers 
        (String.get loc 0, int_of_char (String.get loc 1) - 48))
  | ["UNDER";"ATTACK"] -> PSupport UnderAttack 
  | ["CAN";"ATTACK"] -> PSupport CanAttack 
  | ["SUGGEST"] -> PSupport Suggest
  | _ -> get_hypothetical_cmmd wrd_ls

(** [get_command wrd_ls] is the command that [wrd_ls] represents. 
    Raises: Invalid if [wrd_ls] is not a valid command  *)
let get_command wrd_ls = 
  match wrd_ls with 
  | ["RESIGN"] -> Resign 
  | ["DRAW"] -> Draw 
  | ["HELP"] -> Help 
  | ["CAPTURED"] -> Captured
  | ["SAVE";"AS";s] -> Save s 
  | ["LOG"] -> Log
  | [l1; "TO"; l2] when String.length l1 = 2 && String.length l2 = 2 -> 
    Move (String.get l1 0, int_of_char (String.get l1 1) - 48,
          String.get l2 0, int_of_char (String.get l2 1) - 48 )
  | _ -> get_psupport_cmmd wrd_ls 


let parse p_in = 
  String.split_on_char ' ' p_in 
  |> List.filter (fun x -> x <> "") 
  |> List.map (fun s -> String.uppercase_ascii s)
  |> get_command  




