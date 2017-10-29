test: build decode encode data

build:
	cd src && \
		erlc *.erl

decode: build
	cd src && \
	echo "\
		stackish_test:decode_test(), \
		io:format(\"Passed decode test.~n\"), \
		halt()." | erl

encode: build
	cd src && \
	echo "\
		stackish_test:encode_test(), \
		io:format(\"Passed encode test.~n\"), \
		halt()." | erl

data: build
	cd src && \
		echo "\
		stackish_test:data_test(), \
		halt()." | erl
