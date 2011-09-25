(* 
   Graduate Programming Languages - Wes Weimer
   ASHWIN RAGHAV MOHAN GANESH
   AM2QA
*)

open Re (* re.ml has the definitions for our regexp datatype *) 

(* 
 * "matches re str" returns a full set of suffices 'y' such that
 * 'x''y' = str and re matches 'x'. Thus if re does not match any prefix 
 * of 'str' it should return the emptyset. 
 *)

let rec matches (re : re) (s : re_string) : stringset = match re with
  | Empty -> singleton s 
  | Char(c) -> begin
    match s with
    | c' :: rest when c = c' ->  singleton rest 
    | _ -> emptyset 
  end 
  | Question(re) -> 
	matches (Or(re,Empty)) s
  | AnyChar -> begin
    match s with	
    | c1 :: rest -> singleton rest
    | _ -> emptyset
  end

  | CharRange(from_char, to_char) -> begin
    match s with	
    | c1 :: rest ->
      begin
      match c1 with
      |'a'..'z' -> singleton rest
      | _ -> emptyset
      end
    | _ -> emptyset
  end
  | Concat(re1, re2) -> begin
    Printf.printf "re1 %s" (re_to_str re1);
    Printf.printf "re2 %s" (re_to_str re2);
    Printf.printf "string %s" (re_string_to_str s);
    let set1 = matches re1 s in
    if is_empty set1 then
	emptyset
    else
	begin
    	Printf.printf "re1 is not empty";
	let final_string = Re.fold (fun elt a -> a^Re.re_string_to_str elt) set1 "" in
	matches re2 (string_to_re_string final_string)
	end
  end
  | Or (re1,re2) -> begin
    union (matches re1 s) (matches re2 s);
  end
  | Star(re) ->
		union (singleton s)(matches (Concat(re,Star(re))) s);
(*
  | Plus(re)
*)
  | _ -> Printf.printf "Error: RE %s unimplemented!\n"
    (re_to_str re) ; exit 1 


