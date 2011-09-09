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

test: all test1 test2 test3 test4 

TEST1 = "'print 1 + 2'" 
test1:
	@echo "3" > test-answer
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

# Your project will be graded using: 
#
#	./imp --silent < example-imp-command > test-result
#       diff -b -w test-result test-answer ; then echo "*** passed" ; else
#        echo "*** failed" ; fi 
#
# ... using the "example-imp-command"s submitted in by yourself and others. 
