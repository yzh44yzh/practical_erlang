compile:
	erlc raft.erl

run:
	erl -pa . -name $(NODE)@127.0.0.1 -s raft start

# make run NODE=alpha
# make run NODE=bravo
# make run NODE=charlie
# make run NODE=delta
# make run NODE=echo

test:
	erl -pa . -name test@127.0.0.1 -s raft start
