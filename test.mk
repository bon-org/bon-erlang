test: build decode encode data

build:
	cd src && \
		erlc *.erl

test_bon:
	cd src && \
		echo "\
		c(bon), \
		bon:data_test(), \
		io:format(\"ok~n\"), \
		halt()." | erl

decode: build
	cd src && \
	echo "\
		bon:decode_test(), \
		io:format(\"Passed decode test.~n\"), \
		halt()." | erl

encode: build
	cd src && \
	echo "\
		bon:encode_test(), \
		io:format(\"Passed encode test.~n\"), \
		halt()." | erl

data: build
	cd src && \
		echo "\
		bon:data_test(), \
		halt()." | erl
