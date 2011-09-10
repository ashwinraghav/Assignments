# Graduate Programming Languages - Wes Weimer - Homework 1 Makefile
# 
# if "make" or "gmake" doesn't work for you for some reason, you can
# compile manually like so:
#
# ocamlopt -o imp imp.cmx hw1.cmx parse.cmx lex.cmx main.cmx 
#
# When you're feeling lucky, try "make test" 

OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : imp

hello: hello.cmx
	$(OCAMLOPT) -o hello hello.cmx
	./hello

IMP_OBJS = \
        imp.cmx \
        hw1.cmx \
        parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo lex.ml parse.ml parse.mli imp imp.exe test-result test-answer hello hello.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

imp: $(IMP_OBJS)
	$(OCAMLOPT) -o imp $(IMP_OBJS)

hw1.cmx : hw1.cmi
parse.cmx : parse.cmi parse.ml
main.cmx : hw1.cmi hw1.ml parse.cmi

.PHONY: test1 test2

test: all test1 test2 test3 test4 test5 test6 test7 test8 test9

TEST1 = "'print (1 + 2) * 3'" 
test1:
	@echo "9" > test-answer
	@echo "$(TEST1)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test1 passed" ; else echo "*** test1 FAILED: $(TEST1)" ; fi 

TEST2 = "'print x ; x := 3 ; print x ; x := 4 ; print x'"
test2:
	@echo "0 3 4" > test-answer
	@echo "$(TEST2)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test2 passed" ; else echo "*** test2 FAILED: $(TEST2)" ; fi 

TEST3 = "'{while i <= 5 do { total := total + (i*i) ; i := i + 1 }} ; print total'" 
test3:
	@echo "55" > test-answer
	@echo "$(TEST3)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test3 passed" ; else echo "*** test3 FAILED: $(TEST3)" ; fi 

TEST4 = "'x := 678 ; { let x = 33 in print x } ; print x'"
test4:
	@echo "33 678" > test-answer
	@echo "$(TEST4)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test4 passed" ; else echo "*** test4 FAILED: $(TEST4)" ; fi 

TEST5 = "'x := 678 ; { let x = 33 in let y = x+10 in print y } ; print x'"
test5:
	@echo "43 678" > test-answer
	@echo "$(TEST5)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test5 passed" ; else echo "*** test5 FAILED: $(TEST5)" ; fi 

TEST6 = "' x := 1 ; y := 2 ;{ let x = 3 in print x ; print y ; x := 4 ;y := 5 } ; print x ; print y'"
test6:
	@echo "3215" > test-answer
	@echo "$(TEST6)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test6 passed" ; else echo "*** test6 FAILED: $(TEST6)" ; fi 

TEST7 = "' x := 1 ; y := 2 ;{ let x = 3 in {let x = 6 in print x ; print y ; x := 4 ;y := 5  ; print y } ; print x ; print y};print x'"
test7:
	@echo "625351" > test-answer
	@echo "$(TEST7)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test7 passed" ; else echo "*** test7 FAILED: $(TEST7)" ; fi 

TEST8 = "'i := 0 ; {while  i <= 5 && 5 <= 6  do { x := i; {let i = 6 in print x+i}; i := i+1}}'"
test8:
	@echo "67891011" > test-answer
	@echo "$(TEST8)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test8 passed" ; else echo "*** test8 FAILED: $(TEST8)" ; fi 

TEST9 = "' x := 1 ; y := 2 ;{ let x = 3 in print x ; {let x = x + 2 in print x ; x := 4 ;y := 5  ;print x; print y } ; print x ; print y};print x'"
test9:
	@echo "3545351" > test-answer
	@echo "$(TEST9)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test9 passed" ; else echo "*** test9 FAILED: $(TEST9)" ; fi 
# Your project will be graded using: 
#
#	./imp --silent < example-imp-command > test-result
#       diff -b -w test-result test-answer ; then echo "*** passed" ; else
#        echo "*** failed" ; fi 
#
# ... using the "example-imp-command"s submitted in by yourself and others. 
