(* 
   ASHWIN RAGHAV MOHAN GANESH
   AM2QA
*)

open Re 

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
    | c1 :: rest -> matches(Char(c1)) s
    | _ -> emptyset
  end

  | CharRange(from_char, to_char) -> begin
    match s with	
    (*char range c1..c2 was painful. Could not get it to work. Hence the ugly loop*)
    | c1 :: rest ->
      let rec loop from_c to_c = let ascii1 = Char.code from_c in let ascii2= Char.code to_c in
      if (ascii1 > ascii2) then
	emptyset
      else
        let set1 = (matches (Char(from_c)) s)in
        if is_empty set1 then
          loop (Char.chr (ascii1+1)) to_c
        else
          set1;
      in loop from_char to_char 
    | _ -> emptyset
  end
  | Concat(re1, re2) -> begin
    (*Printf.printf "re1 %s" (re_to_str re1);
    Printf.printf "re2 %s" (re_to_str re2);
    Printf.printf "string %s" (re_string_to_str s); *)
    let set1 = matches re1 s in
    if is_empty set1 then
	emptyset
    else
	begin
    	(*Printf.printf "re1 is not empty";*)
	let final_string = Re.fold (fun elt a -> a^Re.re_string_to_str elt) set1 "" in
	matches re2 (string_to_re_string final_string)
	end
  end
  | Or (re1,re2) ->
    union (matches re1 s) (matches re2 s);
  | Star(re) ->
    union (singleton s)(matches (Concat(re,Star(re))) s);
  | Plus(re)->
    matches (Concat(re,Star(re))) s;	
  | _ -> Printf.printf "Error: RE %s unimplemented!\n"
    (re_to_str re) ; exit 1 
