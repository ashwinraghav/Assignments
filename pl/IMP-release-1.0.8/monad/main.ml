(*
   Test file for evaluating abstract syntax trees of the IMP-language.
   Copyright (C) 1999  Markus Mottl
*)

module MySigma = Sigma.Make (struct type t = string let compare = compare end)
module MyImp = Imp.Make (MySigma)

open MyImp

let _ =
  let print_sigma ch sigma =
    let print (key, value) = Printf.fprintf ch "%s = %d\n" key value in
    List.iter print (MySigma.bindings sigma)

  (* Initial environment *)
  and sigma = MySigma.bind "X" 1024 MySigma.empty

  (* Test case: calculates logarithm to base 2 *)
  and program =
    Seq(
      Seq(
        Assign("Y", Int 0),
        Assign("Z", Int 2)
      ),
      While((Leq (Loc "Z", Loc "X")),
        Seq(
          Assign("Z", Mul (Loc "Z", Int 2)),
          Assign("Y", Add (Loc "Y", Int 1))
        )
      )
    ) in

  Printf.printf "INITIAL ENVIRONMENT\n\n%a\n\nFINAL ENVIRONMENT\n\n%a"
    print_sigma sigma print_sigma (eval program sigma eval_com)
