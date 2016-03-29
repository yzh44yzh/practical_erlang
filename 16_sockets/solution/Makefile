test: build
	erl -pa ebin -s test run

build:
	erlc -o ebin src/*.erl test/*.erl

run:
	erl -pa ebin -s mcache_app start

clean:
	-rm ebin/*.beam
