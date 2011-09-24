{
(* Graduate Programming Languages - Wes Weimer
 * 
 * Lexer for our IMP concrete syntax. 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Lex.
 * See http://en.wikipedia.org/wiki/Lex
 *)
open Parse
} 

let blank = [' ' '\012' '\r' '\t' '\n']

rule initial = parse
  "/*"  { let _ = comment lexbuf in initial lexbuf }
| "(*"  { let _ = comment2 lexbuf in initial lexbuf }
| "//"  { endline lexbuf }
| blank { initial lexbuf }
| '+'           { PLUS }
| '*'           { STAR }
| '-'           { MINUS }
| '.'           { DOT }
| '|'           { BAR }
| '?'           { QUESTION }
| "empty"       { EMPTY }
| "end"         { EOF }

| '('           { LPAREN }
| ')'           { RPAREN } 
| '['           { LBRACKET }
| ']'           { RBRACKET } 
| "'"([^'''] as ch)"'" { CHAR(ch) } 
| '"'([^'"']* as str)'"' { STRING(str) } 

| eof     { EOF } 
| _       { 
  Printf.printf "invalid character '%s'\n" (Lexing.lexeme lexbuf) ;
  (* this is not the kind of error handling you want in real life *)
  exit 1 }

and comment = parse
      "*/"  { () }
|     '\n'  { comment lexbuf }
|     eof   { Printf.printf "unterminated /* comment\n" ; exit 1 }
|     _     { comment lexbuf }
and comment2 = parse
      "*)"  { () }
|     '\n'  { comment2 lexbuf }
|     "(*"  { (* ML-style comments can be nested *) 
              let _ = comment2 lexbuf in comment2 lexbuf }
|     eof   { Printf.printf "unterminated (* comment\n" ; exit 1 }
|     _     { comment2 lexbuf }
and endline = parse
        '\n'      { initial lexbuf}
| _               { endline lexbuf}
|       eof       { EOF }
