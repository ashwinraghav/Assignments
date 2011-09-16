(* 
   IMP - semantics of a simple imperative language
   Copyright (C) 1999  Markus Mottl

   Implements the semantics of the IMP-language as presented in the book
   "The Formal Semantics of Programming Languages" by Glynn Winskel
   (1993, The MIT Press).

   This version explicitely passes the state of the program during
   evaluation.
*)

module type SIGMA = sig
  type 'value state
  type location

  val lookup : location -> 'value state -> 'value option
  val bind : location -> 'value -> 'value state -> 'value state
end

module Make (Sigma : SIGMA) = struct
  type aexp = Int of int
            | Loc of Sigma.location
            | Add of aexp * aexp
            | Sub of aexp * aexp
            | Mul of aexp * aexp

  and bexp  = True
            | False
            | Eq of aexp * aexp
            | Leq of aexp * aexp
            | Not of bexp
            | And of bexp * bexp
            | Or of bexp * bexp

  and com   = Skip
            | Assign of Sigma.location * aexp
            | Seq of com * com
            | Cond of bexp * com * com
            | While of bexp * com

  let rec eval_aexp s = function
    | Int n             -> n
    | Loc l             -> (match Sigma.lookup l s with Some x -> x | _ -> 0)
    | Add(aexp1, aexp2) -> eval_aexp s aexp1 + eval_aexp s aexp2
    | Sub(aexp1, aexp2) -> eval_aexp s aexp1 - eval_aexp s aexp2
    | Mul(aexp1, aexp2) -> eval_aexp s aexp1 * eval_aexp s aexp2

  and eval_bexp s = function
    | True              -> true
    | False             -> false
    | Eq(aexp1, aexp2)  -> eval_aexp s aexp1 = eval_aexp s aexp2
    | Leq(aexp1, aexp2) -> eval_aexp s aexp1 <= eval_aexp s aexp2
    | Not bexp          -> not (eval_bexp s bexp)
    | And(bexp1, bexp2) -> eval_bexp s bexp1 && eval_bexp s bexp2
    | Or(bexp1, bexp2)  -> eval_bexp s bexp1 || eval_bexp s bexp2

  and eval_com s = function
    | Skip             -> s
    | Assign(id, aexp) -> Sigma.bind id (eval_aexp s aexp) s
    | Seq(com1, com2)  -> eval_com (eval_com s com1) com2
    | Cond(bexp, com1, com2) ->
        if eval_bexp s bexp then eval_com s com1 else eval_com s com2
    | While(bexp, com) ->
        let rec loop s' bexp' com' =
          if eval_bexp s' bexp' then loop (eval_com s' com') bexp' com'
          else s' in
        loop s bexp com

  let eval program sigma interpreter = interpreter sigma program
end
