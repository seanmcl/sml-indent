
default : indent test

DEFAULTS := \
-default-ann 'allowFFI false' \
-default-ann 'nonexhaustiveMatch error' \
-default-ann 'nonexhaustiveExnMatch default' \
-default-ann 'redundantMatch error' \
-default-ann 'sequenceNonUnit error' \
-default-ann 'warnUnused true' \
# -default-ann 'forceUsed'

VERBOSE := -verbose 0

MLTON_OPTS := $(VERBOSE) $(DEFAULTS)

MLTON := mlton $(MLTON_OPTS)

indent : indent.lex indent.sml sym.sml run.sml Makefile
	rm -f *.lex.sml
	mllex indent.lex &>/dev/null
	$(MLTON) -const "Exn.keepHistory true" -output $@ sources.mlb

clean :
	-rm -f *.lex.sml *.grm.sml *.grm.sig *.grm.desc indent

.PHONY : test
test :
	cd tests; ./runtests
