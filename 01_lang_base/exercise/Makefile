test:
	erlc test.erl task_1.erl task_2.erl task_3.erl task_4.erl task_5.erl
	erl -s test run

test_1:
	erlc test.erl task_1.erl
	erl -s test run task_1 test

test_2:
	erlc test.erl task_2.erl
	erl -s test run task_2 test

test_3:
	erlc test.erl task_3.erl
	erl -s test run task_3 test

test_4:
	erlc test.erl task_4.erl
	erl -s test run task_4 test

test_5:
	erlc test.erl task_5.erl
	erl -s test run task_5 test


clean:
	-rm *.beam
