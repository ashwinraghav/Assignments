(* Your hw1.ml file must provide these. *) 
type state 
type termination =
  | Normal of state
  | Exceptional of state * Imp.n  
val initial_state : unit -> state
val eval_com : Imp.com -> state -> termination 
