.PHONY: all test eunit check ct compile sh

REBAR=./rebar3

all: compile test

test: check unit ct

compile:
	$(REBAR) compile

ct:
	$(REBAR) ct --config .ct_spec -c true -v

unit:
	$(REBAR) eunit -c true -v

check:
	$(REBAR) dialyzer

sh:
	$(REBAR) shell
