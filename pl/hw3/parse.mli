type token =
  | STRING of (string)
  | CHAR of (char)
  | DOT
  | LBRACKET
  | RBRACKET
  | MINUS
  | EMPTY
  | BAR
  | STAR
  | PLUS
  | QUESTION
  | LPAREN
  | RPAREN
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string * Re.re
