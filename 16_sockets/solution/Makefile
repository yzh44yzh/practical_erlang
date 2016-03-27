compile:
	erlc -o ebin src/*.erl


test: build
	erl -pa ebin -s test run

build:
	erlc -o ebin src/*.erl test/*.erl

clean:
	-rm ebin/*.beam


run:
	erl -pa ebin -s mcache_app start
