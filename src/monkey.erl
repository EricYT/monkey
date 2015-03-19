
-module(monkey).

-export([start/0]).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

start() ->
    io:format("-----------> Monkey king <-------------~n"),
    
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssl),

%%    ensure_started(sync),

    ensure_started(syntax_tools),
    ensure_started(compiler),
    ensure_started(goldrush),
    ensure_started(lager),

    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(cowboy),

    ensure_started(lhttpc),
    ensure_started(emysql),
    ensure_started(reddy),
    ensure_started(ecrontab),
    

    ensure_started(config),
    ensure_started(cache),
    ensure_started(monkey),

    ok.
