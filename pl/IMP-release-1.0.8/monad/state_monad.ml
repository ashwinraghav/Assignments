(* 
   Monads for OCaml
   Copyright (C) 1999  Markus Mottl

   Implementation of State Monad following the paper "Comprehending Monads"
   by Philip Wadler
*)

type ('x, 's) st = 's -> 'x * 's

let (>>=) l r = fun s ->
  let x, s1 = l s in
  let y, s2 = r x s1 in
  y, s2

let (>>) l r = l >>= fun _ -> r

let unitST x = fun s -> x, s

let rec mapST f = function
  | [] -> unitST []
  | h :: t -> f h >>= fun h' ->
                mapST f t >>= fun t' ->
                  unitST (h' :: t')

let rec joinST = function
  | [] -> unitST []
  | h :: t -> h >>= fun x ->
                joinST t >>= fun xs ->
                  unitST (x @ xs)

let fetchST = fun s -> s, s
let assignST s' = fun _ -> (), s'
let initST s x = fst (x s)
