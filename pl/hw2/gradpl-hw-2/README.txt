         Graduate Programming Languages Homework #2 (Code Portion)

-1. This README is just like the README for Homework #1, so you don't need
to read it again if you remember how that worked. 

0. There is a written component to the homework as well. Don't forget!

1. This is version _2_ of the Homework #2 code pack. Before submitting your
work or reporting a bug check to make sure that a more recent version has
not been released. 

2. This assignment will require you to write in OCaml, a popular and
efficient ML variant. ML variants are favored by PL researchers because
they are particularly good for writing programs that manipulate other
programs. 

Manual:         http://caml.inria.fr/pub/docs/manual-ocaml/index.html
Tutorials:      http://caml.inria.fr/pub/docs/manual-ocaml/manual003.html
                http://www.ocaml-tutorial.org/
Download It:    http://caml.inria.fr/download.en.html

In this assignment we'll be manipulating (interpreting) IMP programs. I
have done all of the "undergrad" work (e.g., the lexer, the parser). You
need only flesh out the semantics-based interpreter. 

3. Manifest: 

README.txt              this file
imp.ml                  Winskel's IMP as an abstract data type (AST)
lex.mll                 A "lex" file for our IMP concrete syntax
parse.mly               A "yacc" file for our IMP concrete syntax
main.ml                 A main() driver that reads an IMP command from
                        stdin and evaluates it
hw2.ml                  *** The file you must edit so that it contains
                        an interpreter for IMP with exceptions ***
hw2.mli                 Your hw2.ml must meet this contract (although it
                        can do other things)
hello.ml                "Hello, World" in OCaml
Makefile                "make hello", "make all", "make clean" and "make test"

This distribution ships with parse.ml and lex.ml pre-generated (although
you can rebuild them if you like) so that you can still get going even if
you, for some reason, can't get ocamllex and ocamlyacc to work. 

4. Get ocaml up and running on your system. Make sure that "make hello"
works. Part of comparing and evaluating languages involves being able to 
run and try out new languages and run-time systems. 

5. Run "make all" to build the skeletal IMP interpreter. 

6. Run the resulting imp executable and type in "skip ." as input. You
should see something like this: 

  omoide:~$ ./imp.exe 
  Enter an IMP command (use . to end your command):
  skip .
  skip

The harness accepts an IMP command (terminated with a "."), pretty-prints
it back out, and then interprets it. Interpreting skip isn't that exciting,
however. Our concrete IMP syntax is more or less what you would expect. It
also supports ()'s, {}'s, and comments. Example concrete IMP command:

  x := 5 ;      /* comment */
  { if x <= 9 then { x := x - (5 - 3) } else print x } ; 
  x := 6 
  .

5. Inspect imp.ml and get a feeling for how we have translated IMP into ML.
The translation is quite direct. 

6. Inspect hw2.ml to see the skeletal interpreter. I have provided a
complete interpreter for the Aexp sub-language. You must complete the
skeletal interpreters for Bexp and Com. Use the big-step operational
semantics rules as guides. 

7. Keep at it until you pass all of the tests in "make test". Note that you
should implement the "print" command using something like:

  Printf.printf "%d " the_value

... because the grading script is not expecting you to add newlines. 

8. Write some tests of your own. Put your best test case in the file
"example-imp-command". 

9. Rename "hw2.ml" and "example-imp-command" and submit them as per the
directions in the Homework. 
