        Graduate Programming Languages - Homework #3 (Code Portion)

-1. This README is similar to README for Homework #1, so you don't need
to read it again if you remember how that worked. 

0. There is a written component to the homework as well. Don't forget!

1. This is version _1_ of the Homework #3 code pack. Before submitting your
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

In this assignment we'll be manipulating (interpreting) Regular
Expressions. I have done all of the "undergrad" work (e.g., the lexer, the
parser). You need only flesh out the semantics-based pattern matcher. 

3. Manifest: 

README.txt              this file
re.ml                   Regular Expressions as an ADT 
lex.mll                 A "lex" file for our IMP concrete syntax
parse.mly               A "yacc" file for our IMP concrete syntax
main.ml                 A main() driver that reads an RE and a string 
                        from stdin and evaluates them
hw3.ml                  *** The file you must edit so that it contains
                        an interpreter for REs ***
hw3.mli                 Your hw3.ml must meet this contract (although it
                        can do other things)
test.*                  test cases and answers. 
hello.ml                "Hello, World" in OCaml
Makefile                "make hello", "make all", "make clean" and "make test"

This distribution ships with parse.ml and lex.ml pre-generated (although
you can rebuild them if you like) so that you can still get going even if
you, for some reason, can't get ocamllex and ocamlyacc to work. 

4. Get ocaml up and running on your system. Make sure that "make hello"
works. Part of comparing and evaluating languages involves being able to 
run and try out new languages and run-time systems. 

5. Run "make all" to build the skeletal IMP interpreter. 

6. Run the resulting re executable and type in "skip ." as input. You
should see something like this: 

  omoide:~$ ./re.exe 
  Enter a quoted string and then an RE (type end to end your RE): 
  "hello" empty end 
  empty 
  Matches Leaving "hello" 

The harness accepts a string and a regular expression (terminated with
end), pretty-prints it back out, and then checks to see if the RE matches
the beginning of the string. 

5. Inspect re.ml and get a feeling for how we have translated regular
expressions into ML. The translation is quite direct. 

6. Inspect hw3.ml to see the skeletal interpreter.  You must complete the
skeletal interpreter. Use the denotational semantics rules as guides. 

7. Keep at it until you pass all of the tests in "make test". 

[ My final "matches" procedure was 36 lines of code. ] 

8. Write some tests of your own. Put your best test case in the file
"example-re". 

9. Rename "hw3.ml" and "example-re" and submit them as per the
directions in the Homework. 
