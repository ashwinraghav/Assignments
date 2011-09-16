(* 
   State Monad
   Copyright (C) 1999  Markus Mottl

   Implementation of State Monad following the paper "Comprehending Monads"
   by Philip Wadler
*)

type ('x, 's) st
        (* Type of state monads *)

val unitST : 'x -> ('x, 's) st
        (* [unitST v] returns a computation that computes [v] within
           current state. *)

val mapST :
  (('x, 's1) st -> ('y, 's2) st) -> ('x, 's1) st list -> ('y list, 's2) st
        (* [mapST f l] applies [f] to the result of each (possibly state
           changing) computation in list [l] and returns a computation
           that returns this result. *)

val joinST : ('x list, 's) st list -> ('x list, 's) st
        (* [joinST l] joins a list [l] of (possibly state changing)
           computations that produce lists into one computation that
           produces one list. *)

val (>>=) : ('x, 's) st -> ('x -> ('y, 's) st) -> ('y, 's) st
        (* [l >>= r] evaluates [l] (possibly changing state) and passes
           its value to expression [r]. *)

val (>>) : ('x, 's) st -> ('y, 's) st -> ('y, 's) st
        (* [l >> r] like (>>=), but no value is passed from [l] to [r]. *)

val fetchST : ('s, 's) st
        (* [fetchST] passes the current state as value to the next
           computation. *)

val assignST : 's -> (unit, 's) st
        (* [assignST s] sets the new state to [s], passing the unit
           value to the next computation. *)

val initST : 's -> ('x, 's) st -> 'x
        (* [initST init m] evaluates monadic value (computation) [m]
           with initial state [init] and returns the final value. *)
