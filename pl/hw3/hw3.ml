(* Graduate Programming Languages - Wes Weimer
 * 
 * YOUR NAME HERE (write your name here so that if the files get mixed up
 * somehow I can still give you credit for your work)
 *  
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it. Name the copy "uva_email_addr-hw3.ml". For
 * example, if you are Wes Weimer, send in "wrw6y-hw3.ml". 
 *)

(*
 * Put the code for your interpreter in this file. Your interpreter should
 * be based on the denotational semantics for Regular Expressions that 
 * you developed in the homework. 
 *)

open Re (* re.ml has the definitions for our regexp datatype *) 

(* 
 * "matches re str" returns a full set of suffices 'y' such that
 * 'x''y' = str and re matches 'x'. Thus if re does not match any prefix 
 * of 'str' it should return the emptyset. 
 *)
let rec matches (re : re) (s : re_string) : stringset = match re with
  | Empty -> singleton s 
  | Char(c) -> begin
    match s with
    | c' :: rest when c = c' -> singleton rest 
    | _ -> emptyset 
  end 
  | Question(re) -> matches (Or(re,Empty)) s

  (* you must fill in the other cases here *)

  (* make sure that your program does not loop forever! *) 

  | _ -> Printf.printf "Error: RE %s unimplemented!\n"
    (re_to_str re) ; exit 1 


