%{
(* Graduate Programming Languages - Wes Weimer
 * 
 * Parser for our IMP concrete syntax. 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/manual026.html
 * but basically it works just like Yacc/Bison.
 * See http://en.wikipedia.org/wiki/YACC
 *)

open Imp		    (* IMP abstract syntax 	*)

let error msg	= failwith msg

%}

%token <string>         IDENTIFIER
%token <int>            INT

%token PLUS 
%token MINUS 
%token TIMES 
%token TRUE
%token FALSE
%token EQ_TOK
%token LE_TOK
%token NOT
%token AND
%token OR 
%token SKIP
%token SET 
%token SEMICOLON
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO 
%token TRY
%token CATCH
%token AFTER
%token FINALLY
%token THROW
%token PRINT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token EOF

%start com
%type <Imp.com> com

%left AND
%left OR
%left PLUS MINUS
%left TIMES
%left LE_TOK EQ_TOK
%nonassoc NOT

%%

aexp : INT                                   { Const($1) }
| IDENTIFIER                                 { Var($1) }
| aexp PLUS aexp                             { Add($1,$3) } 
| aexp MINUS aexp                            { Sub($1,$3) } 
| aexp TIMES aexp                            { Mul($1,$3) } 
| LPAREN aexp RPAREN                         { $2 } 
;

bexp : TRUE                                  { True }
| FALSE                                      { False }
| aexp EQ_TOK aexp                           { EQ($1,$3) }
| aexp LE_TOK aexp                           { LE($1,$3) }
| NOT bexp                                   { Not($2) }
| bexp AND bexp                              { And($1,$3) }
| bexp OR bexp                               { Or($1,$3) }
;

com : SKIP                                   { Skip }
| IDENTIFIER SET aexp                        { Set($1,$3) } 
| com SEMICOLON com                          { Seq($1,$3) }
| IF bexp THEN com ELSE com                  { If($2,$4,$6) }
| WHILE bexp DO com                          { While($2,$4) }
| THROW aexp                                 { Throw($2) }
| TRY com CATCH IDENTIFIER com               { TryCatch($2,$4,$5) } 
| AFTER com FINALLY com                      { AfterFinally($2,$4) } 
| PRINT aexp                                 { Print($2) }
| LBRACE com RBRACE                          { $2 } 
;





