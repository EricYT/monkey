%% Feel free to use, reuse and abuse the code in this file.

%% @doc Chunked hello world handler.
-module(toppage_handler).

-export([init/2]).

init(Req, Opts) ->
	Req2 = cowboy_req:chunked_reply(200, Req),
	cowboy_req:chunk("Hello\r\n", Req2),
	timer:sleep(1000),
	cowboy_req:chunk("World\r\n", Req2),
	timer:sleep(1000),
	cowboy_req:chunk("Chunked!\r\n", Req2),
	{ok, Req2, Opts}.
