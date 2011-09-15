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

(* 
 * Our operational semantics has a notion of 'state' (sigma). The type
 * 'state' is a side-effect-ful mapping from 'loc' to 'n'.
 * 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html
 *)
type state = (loc, n) Hashtbl.t

(* 
 * A command may either terminate normally (as before) OR it may terminate
 * exceptionally (and the exception has an integer value).
 *)
type termination = 
  | Normal      of state
  | Exceptional of state * n 

let initial_state () : state = Hashtbl.create 255 

(* Given a state sigma, return the current value associated with
 * 'variable'. For our purposes all uninitialized variables start at 0. *)
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
let rec eval_com (c:com) (sigma:state) : termination = match c with
  | Skip -> (Normal(sigma))
  | _ -> 
    (* you must put real code here *)
    Printf.printf "Warning! Com not yet implemented!\n" ; 
    (Normal(sigma))
