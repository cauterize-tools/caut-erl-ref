.PHONY: compile cover test

all: compile

compile:
	./rebar3 compile
	stack build

cover: test
	./rebar3 cover

test: eunit

eunit:
	./rebar3 do eunit --dir=test, cover

generate:
	stack exec caut-erl-ref -- -s priv/erlang_test.spec -o test
