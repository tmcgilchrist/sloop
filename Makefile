all: compile

compile:
	rebar compile

shell:
	erl -pa ebin -pa include -s reloader
