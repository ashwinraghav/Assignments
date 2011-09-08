(* Graduate Programming Languages - Wes Weimer
 *
 * This is a driver file for the homework 1 project. 
 *
 * Do not edit this file -- instead, edit hw1.ml and submit that as per the
 * directions in the homework. 
 *)
open Printf

let main () = 
  let silent = ref false in 
  let args = [
    "--silent", Arg.Set silent, "do not display a prompt or reminder"
  ] in 
  Arg.parse args (fun _ -> ()) "" ; 

  if not !silent then begin 
    printf "Enter an IMP command (use . to end your command):\n" ;
    flush stdout ; 
  end ; 
  let lexbuf = Lexing.from_channel stdin in
  let imp_command = Parse.com Lex.initial lexbuf in
  if not !silent then begin 
    print_endline (Imp.com_to_str imp_command) ; 
  end ; 
  let sigma_0 = Hw1.initial_state () in 
  let sigma_n = Hw1.eval_com imp_command sigma_0 in
  ignore (sigma_n) ; 
  print_endline "" ; 
  exit 0 
;;
main () ;;