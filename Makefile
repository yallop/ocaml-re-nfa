OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLFIND=ocamlfind

all: lib binary

binary:
	$(OCAMLBUILD) re-nfa-command.native

lib:
	$(OCAMLBUILD) re-nfa.cma re-nfa.cmxa

clean:
	$(OCAMLBUILD) -clean

test: short-tests regenerate-tests
	$(OCAMLBUILD) tests.native
	./tests.native

short-tests:
	$(OCAMLBUILD) tests.native
	./tests.native

regenerate-tests:
	$(OCAMLBUILD) regenerate_tests.native
	./regenerate_tests.native

install: lib binary
	$(OCAMLFIND) install re-nfa META	\
            _build/lib/nfa.cmi			\
            _build/lib/nfa_dot.cmi		\
            _build/lib/regex.cmi		\
            _build/lib/re-nfa.cma		\
            _build/lib/re-nfa.cmxa

uninstall:
	$(OCAMLFIND) remove re-nfa

.PHONY: all lib test clean
