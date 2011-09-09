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
let insert (sigma:state) (variable:loc) (a:aexp) =
    Hashtbl.add sigma variable 1
let lookup (sigma:state) (variable:loc) : n = 
  try
    Hashtbl.find sigma variable 
  with Not_found -> 0 


(* Evaluates an aexp given the state 'sigma'. *) 
let rec eval_aexp (a:aexp) (sigma:state) : n = match a with
  | Const(n) -> n
  | Var(loc) -> lookup sigma loc 
  | Add(a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 + n1
  | Sub(a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 - n1
  | Mul(a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 * n1

(* Evaluates a bexp given the state 'sigma'. *) 
let rec eval_bexp (b:bexp) (sigma:state) : t = match b with
  | True -> true
  | False -> false 
  | _ -> 
    (* you must put real code here *) 
    Printf.printf "Warning! BExp not yet implemented!\n" ; true 

(* Evaluates a com given the state 'sigma'. *) 
let rec eval_com (c:com) (sigma:state) : state = match c with
  | Skip -> sigma
  | Print (a) -> 
	let value = eval_aexp a sigma in 
	Printf.printf "%d" value;sigma
  | Set (loc, a) ->
	insert sigma loc a;
