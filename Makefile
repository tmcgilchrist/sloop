all: compile

compile:
	./rebar compile

deps:
	./rebar get-deps

shell: compile
	erl -pa ebin -pa include -s reloader
