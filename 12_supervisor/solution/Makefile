build: clean
	erlc *.erl

clean:
	-rm *.beam

test: build
	erl -s test run
