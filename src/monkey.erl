
-module(monkey).

-export([start/0]).

start() ->
    io:format("-----------> Monkey king <-------------~n"),
    
    application:start(sasl),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),

%%    application:start(sync),

    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),

    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),

    application:start(lhttpc),
    application:start(emysql),
    application:start(reddy),
    application:start(ecrontab),
    

    application:start(config),
    application:start(cache),
    application:start(monkey),

    ok.
