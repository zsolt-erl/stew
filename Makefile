.PHONY: all compile deps clean test devshell metisshell riakshell bundle release

BRANCH = $(shell git branch|grep '*'|cut -d' ' -f2)
DATE   = $(shell date +%d%b%y)
DIR    = ../RELEASE/
NAME   = phew-branch-$(BRANCH)-$(DATE)


all: compile

compile:
	./rebar compile

clean:
	./rebar clean
	rm -f `find ./ -name '*~'`

