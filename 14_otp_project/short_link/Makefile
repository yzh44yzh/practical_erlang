ERL_RUN_ARGS:=+pc unicode -pa ebin -config short_link -boot start_sasl -s short_link_app start

compile:
	rebar compile skip_deps=true

compile-all:
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

clean-all:
	rebar clean
	rm -f erl_crash.dump

eunit:
	rebar eunit skip_deps=true

run:
	ERL_LIBS=deps erl $(ERL_RUN_ARGS)

background:
	ERL_LIBS=deps erl -detached $(ERL_RUN_ARGS)

d:
	dialyzer --src -I include src

etags:
	etags src/*
