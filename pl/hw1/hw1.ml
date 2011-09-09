(* Graduate Programming Languages - Wes Weimer
 * 
 * Ashwin Raghav Mohan Ganesh
 * UVA Id - am2qa 
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it. Name the copy "your_id-hw1.ml". 
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

(* 
 * Our operational semantics has a notion of 'state' (sigma). The type
 * 'state' is a side-effect-ful mapping from 'loc' to 'n'.
 * 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html
 *)
type state = (loc, n) Hashtbl.t

let initial_state () : state = Hashtbl.create 255 

(* Given a state sigma, return the current value associated with
 * 'variable'. For our purposes all uninitialized variables start at 0. *)
let insert (sigma:state) (variable:loc) (a) : state =
    Hashtbl.add sigma variable a ;sigma

let remove (sigma:state) (variable:loc) : state =
    Hashtbl.remove sigma variable ;sigma

let lookup (sigma:state) (variable:loc) : n = 
  try
    Hashtbl.find sigma variable 
  with Not_found -> 0 


(* Evaluates an aexp given the state 'sigma'. *) 
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
let rec eval_com (c:com) (sigma:state) : state = match c with
  | Skip -> sigma
  | Set(id, aexp) ->
	let value = eval_aexp aexp sigma in
	insert sigma id value;sigma
  | Seq(com1, com2)  -> eval_com com2 (eval_com com1 sigma)
  | If(bexp, com1, com2) ->
      if eval_bexp bexp sigma then eval_com com1 sigma else eval_com com2 sigma
  | While(bexp, com) ->
      let rec loop sigma' bexp' com' =
        if eval_bexp bexp' sigma' then loop (eval_com com' sigma') bexp' com'
        else sigma' in
      loop sigma bexp com
  | Print (a:aexp) ->
        let value = eval_aexp a sigma in
        Printf.printf "%d" value;sigma
  | Let (loc, aexp, com) ->
        let value = eval_aexp aexp sigma in
	insert sigma loc value;
	eval_com com sigma;
	remove sigma loc; sigma
  | _->
	Printf.printf "not yet implemented";sigma
