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
    (*char range c1..c2 was painful. Could not get it to work. Hence the ugly recursion/loop*)
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
    let set1 = matches re1 s in
    if is_empty set1 then
	emptyset
    else
	Re.fold (fun elt a -> union a (matches re2 elt)) set1 emptyset
  end
  | Or (re1,re2) ->
    union (matches re1 s) (matches re2 s);
  | Star(re) ->
    union (singleton s)(matches (Concat(re,Star(re))) s);
  | Plus(re)->
    matches (Concat(re,Star(re))) s;	
  | _ -> Printf.printf "Error: RE %s unimplemented!\n"
    (re_to_str re) ; exit 1 
