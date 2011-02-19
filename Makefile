ERL = erl
ERLC = erlc

compile:
	$(ERLC) -o build build/build.erl
	$(ERL) -noshell -pa build -run build

clean:
	$(RM) ebin/*.beam build/*.beam
	$(RM) include/refac_syntax.hrl
	$(RM) src/refac_syntax_scanner.* src/refac_syntax_parser.*
	$(RM) refactorerl.rel refactorerl.script refactorerl.boot

.PHONY: all compile docs clean
