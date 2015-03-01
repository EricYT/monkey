-module(config).

%% config: config library's entry point.
%% Static config. Useing it by reading the config file

-export([my_func/0]).

-export([get_options/1, get/2, get/3]).


%% private type
-type key_value() :: {atom(), atom()}.

%% API

my_func() ->
    ok().

-spec get_options(Path) -> {ok, Options} | Error when
    Path :: string(),
    Options :: [key_value(),...],
    Error :: atom().
get_options(Path) ->
    case file:consult(Path) of
        {ok, [Res]} -> {ok, Res};
        {error, Error} -> Error
    end.

-spec get(Key, Options) -> Value | undefined when
    Key :: atom(),
    Options :: [key_value(), ...],
    Value :: any().
get(Key, Options) ->
    proplists:get_value(Key, Options).

-spec get(Key, Options, Default) -> Value | Default when
    Key :: atom(),
    Options :: [key_value(), ...],
    Value :: any(),
    Default :: any().
get(Key, Options, Default) ->
    case proplists:get_value(Key, Options) of
        undefined   -> Default;
        Value       -> Value
    end.

%% Internals

ok() ->
    ok.

%% End of Module.
