%% Feel free to use, reuse and abuse the code in this file.

-module(ws_send_many).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
	erlang:send_after(10, self(), send_many),
	{cowboy_websocket, Req, Opts}.

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(send_many, Req, State = [{sequence, Sequence}]) ->
	{reply, Sequence, Req, State}.
