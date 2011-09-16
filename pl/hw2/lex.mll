{
(* 
 * Wes Weimer - CS 655
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
| '-'           { MINUS }
| '*'           { TIMES }
| "true"        { TRUE }
| "false"       { FALSE }
| "="           
| "=="          { EQ_TOK }
| "<="          { LE_TOK }
| '!'           { NOT }
| "&&"
| "/\\"         { AND }
| "||"
| "\\/"         { OR }
| "skip"        { SKIP }
| ":="          { SET }
| ';'           { SEMICOLON }
| "if"          { IF }
| "then"        { THEN }
| "else"        { ELSE }
| "while"       { WHILE }
| "do"          { DO }
| "try"         { TRY }
| "catch"       { CATCH }
| "throw"       { THROW }
| "after"       { AFTER }
| "finally"     { FINALLY }
| "print"       { PRINT }

| '('           { LPAREN }
| ')'           { RPAREN } 
| '{'           { LBRACE }
| '}'           { RBRACE } 

| ("0x")?['0'-'9']+ {
  let str = Lexing.lexeme lexbuf in 
  INT((int_of_string str)) }

| ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
  let str = Lexing.lexeme lexbuf in 
  IDENTIFIER(str)
  } 
| '.' 
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
