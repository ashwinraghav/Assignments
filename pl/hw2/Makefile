# Wes Weimer - Graduate Programming Languages - Homework 2 Makefile
# 
# if "make" or "gmake" doesn't work for you for some reason, you can
# compile manually like so:
#
# ocamlopt -o imp imp.cmx hw2.cmx parse.cmx lex.cmx main.cmx 
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
        hw2.cmx \
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

hw2.cmx : hw2.cmi
parse.cmx : parse.cmi parse.ml
main.cmx : hw2.cmi hw2.ml parse.cmi

.PHONY: test1 test2 test3 test4 test5 test6 test7 test8 test9

test: all test1 test2 test3 test4 test5 test6 test7 test8 test9

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

TEST4 = "'{after x := 2 finally x := 3 } ; print x'"
test4:
	@echo "3" > test-answer
	@echo "$(TEST4)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test4 passed" ; else echo "*** test4 FAILED: $(TEST4)" ; fi 

TEST5 = "'try { x := 3 ; throw x } catch y { print (y+1) }'"
test5:
	@echo "4" > test-answer
	@echo "$(TEST5)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test5 passed" ; else echo "*** test5 FAILED: $(TEST5)" ; fi 

TEST6 = "'throw 7'"
test6:
	@echo "*** Uncaught Exception 7" > test-answer
	@echo "$(TEST6)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test6 passed" ; else echo "*** test6 FAILED: $(TEST6)" ; fi 

TEST7 = "'after print 1 ; throw 8 finally print 2'"
test7:
	@echo "1 2 *** Uncaught Exception 8" > test-answer
	@echo "$(TEST7)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test7 passed" ; else echo "*** test7 FAILED: $(TEST7)" ; fi 

TEST8 = "'x := 1 ; throw 2 ; print x'" 
test8:
	@echo "*** Uncaught Exception 2" > test-answer
	@echo "$(TEST8)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test8 passed" ; else echo "*** test8 FAILED: $(TEST8)" ; fi 

TEST9 = "' x := 0 ;{ try if x <= 5 then throw 33 else throw 55 catch x print x } ; while true do { x := x - 15 ; print x ; if x <= 0 then throw (x*2) else skip }'" 
test9:
	@echo "33183-12 *** Uncaught Exception -24" > test-answer
	@echo "$(TEST9)" | ./imp --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test9 passed" ; else echo "*** test9 FAILED: $(TEST8)" ; fi 

# Your project will be graded using: 
#
#	./imp --silent < example-imp-command > test-result
#       diff -b -w test-result test-answer ; then echo "*** passed" ; else
#        echo "*** failed" ; fi 
#
# ... using the "example-imp-command"s submitted in by yourself and others. 
