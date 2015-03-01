-module(cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("cache.hrl").

%% record define
-record(state, {redis_poolids = gb_trees:empty()}).



%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    %% Get configs
    {ok, Options}   = config:get_options(?CONFIG),
    Host            = config:get(host, Options, "localhost"),
    Password        = config:get(password, Options, undefined),
    Port            = config:get(posrt, Options, 6379),
    
    %% put pool 
    PPoolid         = ?PPOOLID,
    PCount          = config:get(pcount, Options, 10),

    %% get pool
    GPoolid         = ?GPOOLID,
    GCount          = config:get(gcount, Options, 10),

    reddy_pool:new_pool(PPoolid, [{ip, Host}, {pass, Password}, {port, Port}, {count, PCount}]),
    reddy_pool:new_pool(GPoolid, [{ip, Host}, {pass, Password}, {port, Port}, {count, GCount}]),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

