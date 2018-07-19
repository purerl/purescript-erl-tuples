.PHONY: ps erl all test

all: test

ps:
	psc-package sources | xargs purs compile 'src/**/*.purs' 'test/**/*.purs'

erl: ps
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

test: erl
	erl -pa ebin -noshell -eval '(test_main@ps:main@c())()' -eval 'init:stop()'