test: build
	erl -s test run

build: clean
	erlc great_ideas_catalogue.erl great_ideas_catalogue_test.erl test.erl

clean:
	-rm *.beam

test_1: build
	erl -s test run great_ideas_catalogue_test add_idea_test

test_2: build
	erl -s test run great_ideas_catalogue_test get_idea_test

test_3: build
	erl -s test run great_ideas_catalogue_test ideas_by_author_test

test_4: build
	erl -s test run great_ideas_catalogue_test ideas_by_rating_test

test_5: build
	erl -s test run great_ideas_catalogue_test get_authors_test
