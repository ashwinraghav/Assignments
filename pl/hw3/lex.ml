# 1 "lex.mll"
 
(* Graduate Programming Languages - Wes Weimer
 * 
 * Lexer for our IMP concrete syntax. 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Lex.
 * See http://en.wikipedia.org/wiki/Lex
 *)
open Parse

# 13 "lex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\236\255\237\255\001\000\005\000\240\255\241\255\242\255\
    \000\000\246\255\247\255\248\255\249\255\250\255\251\255\252\255\
    \006\000\007\000\253\255\255\255\254\255\000\000\000\000\000\000\
    \000\000\245\255\244\255\011\000\239\255\238\255\002\000\009\000\
    \252\255\253\255\254\255\005\000\255\255\013\000\251\255\252\255\
    \014\000\254\255\016\000\255\255\253\255\004\000\253\255\254\255\
    \255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\019\000\019\000\255\255\255\255\255\255\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \012\000\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\003\000\255\255\255\255\255\255\255\255\
    \004\000\255\255\004\000\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\030\000\027\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\030\000\032\000\
    \000\000\000\000\000\000\255\255\000\000\038\000\000\000\000\000\
    \255\255\000\000\255\255\000\000\000\000\047\000\000\000\000\000\
    \000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\015\000\000\000\015\000\015\000\048\000\000\000\
    \000\000\000\000\000\000\034\000\000\000\000\000\000\000\041\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\000\000\003\000\029\000\029\000\000\000\000\000\004\000\
    \016\000\007\000\013\000\014\000\255\255\012\000\011\000\017\000\
    \020\000\019\000\028\000\035\000\036\000\040\000\018\000\042\000\
    \044\000\043\000\000\000\000\000\000\000\000\000\000\000\009\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\026\000\008\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\022\000\021\000\000\000\
    \023\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
    \000\000\025\000\000\000\000\000\010\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\255\255\000\000\046\000\255\255\000\000\000\000\
    \000\000\033\000\000\000\000\000\000\000\039\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\000\000\000\000\045\000\255\255\
    \255\255\255\255\255\255\031\000\255\255\255\255\255\255\037\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\003\000\030\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\
    \016\000\017\000\027\000\031\000\035\000\037\000\017\000\037\000\
    \040\000\042\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\021\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\008\000\008\000\255\255\
    \022\000\255\255\255\255\255\255\023\000\255\255\255\255\255\255\
    \255\255\024\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\003\000\030\000\255\255\045\000\004\000\255\255\255\255\
    \255\255\031\000\255\255\255\255\255\255\037\000\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec initial lexbuf =
    __ocaml_lex_initial_rec lexbuf 0
and __ocaml_lex_initial_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 15 "lex.mll"
        ( let _ = comment lexbuf in initial lexbuf )
# 132 "lex.ml"

  | 1 ->
# 16 "lex.mll"
        ( let _ = comment2 lexbuf in initial lexbuf )
# 137 "lex.ml"

  | 2 ->
# 17 "lex.mll"
        ( endline lexbuf )
# 142 "lex.ml"

  | 3 ->
# 18 "lex.mll"
        ( initial lexbuf )
# 147 "lex.ml"

  | 4 ->
# 19 "lex.mll"
                ( PLUS )
# 152 "lex.ml"

  | 5 ->
# 20 "lex.mll"
                ( STAR )
# 157 "lex.ml"

  | 6 ->
# 21 "lex.mll"
                ( MINUS )
# 162 "lex.ml"

  | 7 ->
# 22 "lex.mll"
                ( DOT )
# 167 "lex.ml"

  | 8 ->
# 23 "lex.mll"
                ( BAR )
# 172 "lex.ml"

  | 9 ->
# 24 "lex.mll"
                ( QUESTION )
# 177 "lex.ml"

  | 10 ->
# 25 "lex.mll"
                ( EMPTY )
# 182 "lex.ml"

  | 11 ->
# 26 "lex.mll"
                ( EOF )
# 187 "lex.ml"

  | 12 ->
# 28 "lex.mll"
                ( LPAREN )
# 192 "lex.ml"

  | 13 ->
# 29 "lex.mll"
                ( RPAREN )
# 197 "lex.ml"

  | 14 ->
# 30 "lex.mll"
                ( LBRACKET )
# 202 "lex.ml"

  | 15 ->
# 31 "lex.mll"
                ( RBRACKET )
# 207 "lex.ml"

  | 16 ->
let
# 32 "lex.mll"
                ch
# 213 "lex.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 32 "lex.mll"
                       ( CHAR(ch) )
# 217 "lex.ml"

  | 17 ->
let
# 33 "lex.mll"
                 str
# 223 "lex.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 33 "lex.mll"
                         ( STRING(str) )
# 227 "lex.ml"

  | 18 ->
# 35 "lex.mll"
          ( EOF )
# 232 "lex.ml"

  | 19 ->
# 36 "lex.mll"
          ( 
  Printf.printf "invalid character '%s'\n" (Lexing.lexeme lexbuf) ;
  (* this is not the kind of error handling you want in real life *)
  exit 1 )
# 240 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_initial_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 31
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 42 "lex.mll"
            ( () )
# 251 "lex.ml"

  | 1 ->
# 43 "lex.mll"
            ( comment lexbuf )
# 256 "lex.ml"

  | 2 ->
# 44 "lex.mll"
            ( Printf.printf "unterminated /* comment\n" ; exit 1 )
# 261 "lex.ml"

  | 3 ->
# 45 "lex.mll"
            ( comment lexbuf )
# 266 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and comment2 lexbuf =
    __ocaml_lex_comment2_rec lexbuf 37
and __ocaml_lex_comment2_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 47 "lex.mll"
            ( () )
# 277 "lex.ml"

  | 1 ->
# 48 "lex.mll"
            ( comment2 lexbuf )
# 282 "lex.ml"

  | 2 ->
# 49 "lex.mll"
            ( (* ML-style comments can be nested *) 
              let _ = comment2 lexbuf in comment2 lexbuf )
# 288 "lex.ml"

  | 3 ->
# 51 "lex.mll"
            ( Printf.printf "unterminated (* comment\n" ; exit 1 )
# 293 "lex.ml"

  | 4 ->
# 52 "lex.mll"
            ( comment2 lexbuf )
# 298 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment2_rec lexbuf __ocaml_lex_state

and endline lexbuf =
    __ocaml_lex_endline_rec lexbuf 45
and __ocaml_lex_endline_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 54 "lex.mll"
                  ( initial lexbuf)
# 309 "lex.ml"

  | 1 ->
# 55 "lex.mll"
                  ( endline lexbuf)
# 314 "lex.ml"

  | 2 ->
# 56 "lex.mll"
                  ( EOF )
# 319 "lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_endline_rec lexbuf __ocaml_lex_state

;;
