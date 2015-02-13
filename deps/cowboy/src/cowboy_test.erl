
-module(cowboy_test).

-export([execute/2]).

execute(Req, Env) ->
    io:format("-----> Req:~p Env:~p ~n", [Req, Env]),
    {ok, Req, Env}.
