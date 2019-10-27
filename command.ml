
type location = char * int 
type t = 
  | Quit 
  | Draw 
  | Move of location*location
exception Invalid 

val parse : string -> t 



