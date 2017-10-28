test: build decode encode

build:
	cd src && \
		erlc *.erl

decode: build
	cd src && \
		#stackish_test:start_trace(), \
	echo "\
		stackish_test:decode_test(), \
		io:format(\"Passed decode test.~n\"), \
		halt()." | erl

encode: build
	cd src && \
		#stackish_test:start_trace(), \
	echo "\
		stackish_test:encode_test(), \
		io:format(\"Passed encode test.~n\"), \
		halt()." | erl
