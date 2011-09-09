type token =
  | IDENTIFIER of (string)
  | INT of (int)
  | PLUS
  | MINUS
  | TIMES
  | TRUE
  | FALSE
  | EQ_TOK
  | LE_TOK
  | NOT
  | AND
  | OR
  | SKIP
  | SET
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | LET
  | IN
  | PRINT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EOF

val com :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Imp.com
