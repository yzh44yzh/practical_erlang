ERL_RUN_ARGS:=+pc unicode -pa ebin -config my_crypt -boot start_sasl -s my_crypt_app start

compile:
	erlc -I include -o ebin src/*.erl

clean:
	-rm ebin/*.beam

run:
	ERL_LIBS=deps erl $(ERL_RUN_ARGS)
