test: build decode

build:
	cd src && \
		erlc *.erl

decode: build
	cd src && \
		#stackish_test:start_trace(), \
	echo "\
		stackish_test:decode_test(), \
		io:format(\"All Passed.~n\"), \
		halt()." | erl
