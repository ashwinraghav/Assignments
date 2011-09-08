(* Your hw1.ml file must provide these. *) 
type state 
val initial_state : unit -> state
val eval_com : Imp.com -> state -> state 
