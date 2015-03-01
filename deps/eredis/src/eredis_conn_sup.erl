%% Copyright (c) 2011 Kevin Smith <kevin@hypotheticalabs.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(eredis_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_conn/1]).

-define(SERVER, ?MODULE).
%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_conn(Opts) ->
    Host           = proplists:get_value(host, Opts, "127.0.0.1"),
    Port           = proplists:get_value(port, Opts, 6379),
    Database       = proplists:get_value(database, Opts, 0),
    Password       = proplists:get_value(password, Opts, ""),
    ReconnectSleep = proplists:get_value(reconnect_sleep, Opts, 100),
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, ?TIMEOUT),
    supervisor:start_child(?SERVER, [Host, Port, Database, Password, ReconnectSleep, ConnectTimeout]).

init([]) ->
    Child = {eredis_client, {eredis_client, start_link, []},
             temporary, brutal_kill, worker, [eredis_client]},
    Strategy = {simple_one_for_one, 0, 1},
    {ok, {Strategy, [Child]}}.
