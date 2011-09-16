(* Graduate Programming Languages - Wes Weimer
 * 
 * YOUR NAME HERE (write your name here so that if the files get mixed up
 * somehow I can still give you credit for your work)
 *  
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it. Name the copy "uva_email_address-hw2.ml". For
 * example, if you are Grace Hopper, send in "gmh1y-hw2.ml". 
 *)

(*
 * Put the code for your interpreter in this file. Your interpreter should
 * be based on the large-step (natural-style) operational semantics for IMP
 * that we went over in class (and in Winskel's book). 
 *
 * This skeleton file includes one implementation of states (based on
 * OCaml's Hashtbl) and evaluations for AExps. 
 *)

open Imp (* imp.ml has the definitions for our IMP datatypes *) 

type state = (loc, n) Hashtbl.t

type termination = 
  | Normal      of state
  | Exceptional of state * n 

let initial_state () : state = Hashtbl.create 255

let find_all (sigma:state) (variable:loc) =
    Hashtbl.find_all sigma variable

let replace (sigma:state) (variable:loc) (a) : state =
    Hashtbl.replace sigma variable a ;sigma

let remove (sigma:state) (variable:loc) : state =
    Hashtbl.remove sigma variable ;sigma

let lookup (sigma:state) (variable:loc) : n =
  try
    Hashtbl.find sigma variable
  with Not_found -> 0

let rec eval_aexp (a:aexp) (sigma:state) : n = match a with
  | Const(n) -> n
  | Var(loc) -> lookup sigma loc
  | Add(a0,a1) -> eval_aexp a0 sigma + eval_aexp a1 sigma
  | Sub(a0,a1) -> eval_aexp a0 sigma - eval_aexp a1 sigma
  | Mul(a0,a1) -> eval_aexp a0 sigma * eval_aexp a1 sigma

(* Evaluates a bexp given the state 'sigma'. *)
let rec eval_bexp (b:bexp) (sigma:state) : t = match b with
  | True              -> true
  | False             -> false
  | EQ(aexp1, aexp2)  -> eval_aexp aexp1 sigma= eval_aexp aexp2 sigma
  | LE(aexp1, aexp2) -> eval_aexp aexp1 sigma <= eval_aexp aexp2 sigma
  | Not bexp          -> not (eval_bexp bexp sigma)
  | And(bexp1, bexp2) -> eval_bexp bexp1 sigma && eval_bexp bexp2 sigma
  | Or(bexp1, bexp2)  -> eval_bexp bexp1 sigma || eval_bexp bexp2 sigma

(* Evaluates a com given the state 'sigma'. *) 
let rec eval_com (c:com) (sigma:state) : termination = match c with
  | Skip -> (Normal(sigma))
  | Set(id, aexp) ->
        let value = eval_aexp aexp sigma in
        replace sigma id value;
	Normal(sigma);
  | Seq(com1, com2)  ->
	begin 
	let termination1 = eval_com (com1 sigma) in 
	let final_termination = (match termination1 with
	|Normal(sigma) ->
		eval_com com2 sigma;
	|Exceptional(state, value) ->
		Exceptional(state, value);
	|_ -> Normal(sigma);)
	end
  | If(bexp, com1, com2) ->
      if eval_bexp bexp sigma then eval_com com1 sigma else eval_com com2 sigma
  | While(bexp, com) ->
      (*This is going to change as well*)
      let rec loop sigma' bexp' com' =
        if eval_bexp bexp' sigma' then loop (eval_com com' sigma') bexp' com'
        else (Normal(sigma')) in
      loop sigma bexp com
  | Print (a:aexp) ->
        let value = eval_aexp a sigma in
        Printf.printf "%d" value;
  | Throw (aexp) ->
	let exception_value = eval_aexp aexp in
	(Exceptional(sigma, exception_value));
  | TryCatch (com1, loc, com2) ->
	(match (eval_com com1) with
		|Normal (sigma) ->
			Normal(sigma);
		|Exceptional(sigma, exception_value) ->
        		Normal(replace sigma loc exception_value))
  | AfterFinally(com1, com2) ->
	let termination1 = eval_com com1 in
	let termination2 = eval_com com2 in
	(match termination2 with
	|Exceptional(state_2, exception_value_2) ->
		Exceptional(state_2, exception_value_2);
	|Normal (state_2) ->
		(match termination1 with
		|Exceptional(state_1, exception_value_1) ->
			Exceptional(state_1, exception_value_1);
		|Normal(state_1) ->
			Normal(state_2)))

				
		
	
