.PHONY: ps erl all test

all: ps erl

ps:
	psc-package sources | xargs pserlc 'src/**/*.purs'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
