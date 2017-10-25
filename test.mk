test: build decode

build:
	cd src && \
		erlc *.erl

decode: build
	cd src && \
	echo "\
		stackish_test:start_trace(), \
		stackish_test:decode_test(). \
		" | erl
