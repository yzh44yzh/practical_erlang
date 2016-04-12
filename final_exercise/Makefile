#test: build
#	erl -pa ebin -s test run

build:
	erlc -o ebin src/*.erl test/*.erl

run:
	erl -pa ebin #-s game start

clean:
	-rm ebin/*.beam

d:
	dialyzer --src src

telnet:
	telnet localhost 1234
