
-module(cache).

-include("cache.hrl").

-export([
    put/2, put/3, put_sync/2, puts/2, puts/1, puts_sync/1,
    get/1, get_async/2, gets/1
    ]).

-type key() :: string() | binary() | integer().
-type value() :: string() | binary() | integer().
-type key_value() :: {key(), value()}.


-spec put(Key, Value) -> ok when
    Key :: key(),
    Value :: value().
put(Key, Value) ->
    reddy_strings:set_(?PPOOLID, Key, Value, false).

-spec put(Key, Value, WantsReturn) -> Res when
    Key :: key(),
    Value :: value(),
    WantsReturn :: boolean(),
    Res :: any().
put(Key, Value, WantsReturn) when is_boolean(WantsReturn) ->
    reddy_strings:set_(?PPOOLID, Key, Value, WantsReturn).

-spec put_sync(Key, Value) -> ok | Error when
    Key :: key(),
    Value :: value(),
    Error :: any().
put_sync(Key, Value) ->
    reddy_strings:set(?PPOOLID, Key, Value).


-spec puts(KeyValues) -> ok when
    KeyValues :: [key_value(), ...].
puts(KeyValues) when is_list(KeyValues) ->
    reddy_strings:mset_(?PPOOLID, KeyValues, false).

-spec puts(KeyValues, WantsReturn) -> ok when
    KeyValues :: [key_value(), ...],
    WantsReturn :: boolean().
puts(KeyValues, WantsReturn) when is_list(KeyValues), is_boolean(WantsReturn) ->
    reddy_strings:mset_(?PPOOLID, KeyValues, WantsReturn).

-spec puts_sync(KeyValues) -> Res when
    KeyValues :: [key_value(), ...],
    Res :: any().
puts_sync(KeyValues) when is_list(KeyValues) ->
    reddy_strings:mset(?PPOOLID, KeyValues).


-spec get(Key) -> Value | undefined when
    Key :: key(),
    Value :: value().
get(Key) ->
    reddy_strings:get(?GPOOLID, Key).

-spec get_async(Key, WantsReturn) -> Value | undefined when
    Key :: key(),
    WantsReturn :: boolean(),
    Value :: value().
get_async(Key, WantsReturn) when erlang:is_boolean(WantsReturn) ->
    reddy_strings:get_(?GPOOLID, Key, WantsReturn).

-spec gets(Keys) -> Values when
    Keys :: [key(), ...],
    Values :: [value(), ...].
gets(Keys) when is_list(Keys) ->
    reddy_strings:mget(?GPOOLID, Keys).
