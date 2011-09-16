(* 
   Simple & not efficient implementation of environments
   Copyright (C) 1999  Markus Mottl
*)

(* The functor [Make] takes a module containing information on the type
   and ordering semantics of identifiers. *)
module Make (Loc : sig type t val compare : t -> t -> int end) : sig
  type 'value state
        (* Type of environments *)
  type location = Loc.t
        (* Type of identifiers *)

  val lookup : location -> 'value state -> 'value option
        (* [lookup id s] returns [Some value] associated to identifier
           [id] in environment [s] or [None] if no such binding exists. *)

  val bind : location -> 'value -> 'value state -> 'value state
        (* [bind id v s] binds identifier [id] to value [s] in environment
           [s] and returns the new environment. The old environment is
           unchanged! *)

  val bindings : 'value state -> (location * 'value) list
        (* [bindings s] returns a list of (identifier, value)-tuples of
           all bindings in environment [s]. *)

  val empty : 'value state
        (* [empty] is the empty environment. *)
end =
struct
  module IdMap = Map.Make (struct type t = Loc.t let compare = compare end)

  type 'value state = 'value IdMap.t
  type location = Loc.t

  let bind = IdMap.add
  let lookup k s = try Some (IdMap.find k s) with Not_found -> None
  let empty = IdMap.empty

  let bindings s = IdMap.fold (fun key value accu -> (key, value) :: accu) s []
end
