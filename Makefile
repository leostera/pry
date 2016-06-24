.PHONY: all test eunit check ct compile sh clean

REBAR=./rebar3

all: compile test

test: check unit ct

compile:
	$(REBAR) compile

ct:
	$(REBAR) ct --config .ct_spec -c true

unit:
	$(REBAR) eunit -c true

check:
	$(REBAR) dialyzer

sh:
	$(REBAR) shell

clean:
	$(REBAR) clean
	rm -rf _build
