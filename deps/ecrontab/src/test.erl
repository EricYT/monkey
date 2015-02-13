%% =============================================================================
%%  Copyright (c) 2013.
%% =============================================================================

%%%----------------------------------------------------------------------
%%%
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the test module
%%%
%%%----------------------------------------------------------------------
-module(test).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("ecrontab.hrl").

-compile([export_all]).


run(N) ->
    io:format("run the routine by cron server:~p~n", [N]).
