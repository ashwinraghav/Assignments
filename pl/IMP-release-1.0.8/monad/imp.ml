(* 
   IMP - semantics of a simple imperative language
   Copyright (C) 1999  Markus Mottl

   Implements the semantics of the IMP-language as presented in the book
   "The Formal Semantics of Programming Languages" by Glynn Winskel
   (1993, The MIT Press).

   This version is in monadic style.
*)

module type SIGMA = sig
  type 'value state
  type location

  val lookup : location -> 'value state -> 'value option
  val bind : location -> 'value -> 'value state -> 'value state
end

module Make (Sigma : SIGMA) = struct
  open State_monad

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

  let rec eval_aexp = function
    | Int n             -> unitST n
    | Loc l             -> fetchST >>= fun s ->
                             unitST (match Sigma.lookup l s with
                                     | Some a -> a | None -> 0)
    | Add(aexp1, aexp2) -> eval_aexp aexp1 >>= fun a ->
                             eval_aexp aexp2 >>= fun b -> unitST (a + b)
    | Sub(aexp1, aexp2) -> eval_aexp aexp1 >>= fun a ->
                             eval_aexp aexp2 >>= fun b -> unitST (a - b)
    | Mul(aexp1, aexp2) ->  eval_aexp aexp1 >>= fun a ->
                             eval_aexp aexp2 >>= fun b -> unitST (a * b)

  and eval_bexp = function
    | True              -> unitST true
    | False             -> unitST false
    | Eq(aexp1, aexp2)  -> eval_aexp aexp1 >>= fun a ->
                             eval_aexp aexp2 >>= fun b -> unitST (a = b)
    | Leq(aexp1, aexp2) -> eval_aexp aexp1 >>= fun a ->
                             eval_aexp aexp2 >>= fun b -> unitST (a <= b)
    | Not bexp          -> eval_bexp bexp >>= fun bexp' -> unitST (not bexp')
    | And(bexp1, bexp2) -> eval_bexp bexp1 >>= fun a ->
                             if a then eval_bexp bexp2 >>= fun b ->
                               unitST (a && b)
                             else unitST false
    | Or(bexp1, bexp2)  -> eval_bexp bexp1 >>= fun a ->
                             if a then unitST true
                             else eval_bexp bexp2 >>= fun b -> unitST (a || b)

  and eval_com = function
    | Skip             -> fetchST
    | Assign(id, aexp) -> eval_aexp aexp >>= fun value -> fetchST >>= fun s ->
                            assignST (Sigma.bind id value s) >> fetchST
    | Seq(com1, com2)  -> eval_com com1 >> eval_com com2
    | Cond(bexp, com1, com2) ->
        eval_bexp bexp >>= fun is_true ->
          if is_true then eval_com com1 else eval_com com2
    | While(bexp, com) ->
        let rec loop bexp' com' =
          eval_bexp bexp' >>=
            function true -> eval_com com' >> loop bexp' com' | _ -> fetchST in
        loop bexp com

  let eval program sigma interpreter = initST sigma (interpreter program)
end
