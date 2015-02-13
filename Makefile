
PROJECT= millapp
DEPS= cowboy

all: 
	rebar compile


run: 
	@./start_up.sh

eunit: 
	rebar eunit
