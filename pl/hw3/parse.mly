%{
(* 
 * Wes Weimer - Graduate Programming Languages
 * 
 * Parser for our RE concrete syntax. 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Yacc/Bison.
 * See http://en.wikipedia.org/wiki/YACC
 *)

open Re		    (* RE abstract syntax 	*)

let error msg	= failwith msg

%}

%token <string>         STRING
%token <char>           CHAR
%token DOT
%token LBRACKET
%token RBRACKET
%token MINUS
%token EMPTY
%token BAR
%token STAR
%token PLUS
%token QUESTION

%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN

%token EOF

%start program
%type <string * Re.re> program

%left QUESTION PLUS TIMES 
%right CONCAT
%left OR

%%

program : STRING re EOF { ($1, $2) }
;

re : LPAREN re RPAREN     { $2 }
| CHAR                    { Char($1) }
| STRING        
  { 
    let char_list = string_to_re_string $1 in 
    match char_list with
    | [] -> error "you may not use an empty string in a regular expression" 
    | hd :: tl -> begin
      List.fold_left (fun acc elt -> Concat(acc,Char(elt)))
        (Char(hd)) tl 
    end 
  } 
| EMPTY                   { Empty } 
| DOT                     { AnyChar } 
| LBRACKET CHAR MINUS CHAR RBRACKET 
    { 
      let c1 = $2 in
      let c2 = $4 in
      if c1 < c2 then CharRange(c1,c2)
      else CharRange(c2,c1) 
    } 
| re %prec CONCAT re     { Concat($1,$2) }
| re BAR re              { Or($1,$3) }
| re STAR                { Star($1) }
| re PLUS                { Plus($1) }
| re QUESTION            { Question($1) }
;


