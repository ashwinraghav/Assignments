# Wes Weimer - Graduate Programming Languages - Makefile
# 
# if "make" or "gmake" doesn't work for you for some reason, you can
# compile manually like so:
#
# ocamlopt -o re re.cmx hw3.cmx parse.cmx lex.cmx main.cmx 
#
# When you're feeling lucky, try "make test" 

OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : re

hello: hello.cmx
	$(OCAMLOPT) -o hello hello.cmx
	./hello

RE_OBJS = \
        re.cmx \
        hw3.cmx \
        parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo lex.ml parse.ml parse.mli re re.exe *.result hello hello.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

re: $(RE_OBJS)
	$(OCAMLOPT) -o re $(RE_OBJS)

hw3.cmx : hw3.cmi
parse.cmx : parse.cmi parse.ml
main.cmx : hw3.cmi hw3.ml parse.cmi

test: all \
        test1.test test2.test test3.test test4.test test5.test test6.test \
        test7.test test8.test test9.test 

%.test: %.re
	@./re --silent < $< > $<.result
	@if diff -b -w $<.answer $<.result ; then echo "*** $< passed" ; else echo "*** $< FAILED:" ; cat $< ; fi 


# Your project will be graded using: 
#
#	./re --silent < example-re > test-result
#       diff -b -w test-result test-answer ; then echo "*** passed" ; else
#        echo "*** failed" ; fi 
#
# ... using the "example-re"s submitted in by yourself and others. 
