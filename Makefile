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

test: builder
	$(ERL) -noshell -pa build -s build start export_all

doc: builder
	$(ERL) -noshell -pa build -run build doc -run init stop

clean:
	$(RM) lib/*/ebin/*.beam build/*.beam build/leex/ebin/*.beam
	$(RM) lib/refactorerl/include/referl_syntax.hrl
	$(RM) lib/refactorerl/src/referl_syntax_*.*
	$(RM) lib/refactorerl/priv/erlang.lex*
	$(RM) refactorerl.rel refactorerl.script refactorerl.boot

.PHONY: default all tool doc builder clean
