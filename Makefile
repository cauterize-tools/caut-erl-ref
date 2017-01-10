.PHONY: compile cover test crucible

all: compile

compile:
	./rebar3 compile
	stack build

cover: test
	./rebar3 cover

test: eunit crucible

eunit: generate
	./rebar3 do eunit --dir=test, cover

generate:
	stack exec caut-erl-ref -- -s priv/erlang_test.spec -o test

crucible:
	./run_crucible.sh
