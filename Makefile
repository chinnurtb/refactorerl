OTP_BIN = 

ERL = $(OTP_BIN)erl
ERLC = $(OTP_BIN)erlc

default: tool
all: tool doc

builder: build/build.beam
build/build.beam: build/build.erl
	$(ERLC) -o build build/build.erl

tool: builder
	$(ERL) -noshell -pa build -run build

doc: builder
	$(ERL) -noshell -pa build -run build doc -run init stop

clean:
	$(RM) lib/*/ebin/*.beam build/*.beam
	$(RM) lib/refactorerl/include/refac_syntax.hrl
	$(RM) lib/refactorerl/src/refac_syntax_scanner.*
	$(RM) lib/refactorerl/src/refac_syntax_parser.*
	$(RM) refactorerl.rel refactorerl.script refactorerl.boot

.PHONY: default all tool doc builder clean
