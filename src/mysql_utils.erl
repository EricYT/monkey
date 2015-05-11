
-module(mysql_utils).

-export([select/3, insert/3, update/3]).

-define(COMMA_SEP, ", ").
-define(AND_SEP, " and ").
-define(IS_SEP, " is ").
-define(EQU_SEP, " = ").

%% make sqls

-spec select(Selects, Conditions, Table) -> Res when
  Selects :: Keys,
  Keys :: [atom(), ...],
  Conditions :: [{Key, Value}, ...],
  Key :: atom(),
  Value :: any(),
  Table :: atom(),
  Res :: list().
select(Selects, Conditions, Table) ->
  Table_ = atom_to_list(Table),
  Selects_ = case Selects of
    [] -> "*";
    _  -> make_keys(Selects)
  end,
  Conditions_ = make_conditions(Conditions),
  make_flattent_list("select ~s from ~s where ~s", [Selects_, Table_, Conditions_]).

insert(Contents, Conditions, Table) ->
  Table_     = atom_to_list(Table),
  Conditions_= make_conditions(Conditions),
  Sets       = make_sets(Contents),
  make_flattent_list("insert into ~s set ~s where ~s", [Table_, Sets, Conditions_]).

update(Contents, Conditions, Table) ->
  Table_     = atom_to_list(Table),
  Conditions_= make_conditions(Conditions),
  Sets       = make_sets(Contents),
  make_flattent_list("update ~s set ~s where ~s", [Table_, Sets, Conditions_]).

%%
%% Internal functions
%%
make_flattent_list(Format, Values) ->
  lists:flatten(io_lib:format(Format, Values)).

concat([Key], Acc, _Seperator) ->
  concat([], [Key|Acc], _Seperator);
concat([Key|Tail], Acc, Seperator) ->
  concat(Tail, [Seperator, Key|Acc], Seperator);
concat([], Acc, _) -> lists:concat(lists:reverse(Acc)).

make_keys(Keys) ->
  concat(Keys, [], ?COMMA_SEP).

make_conditions(Conditions) ->
  concat(make_key_values(Conditions, [], ?IS_SEP), [], ?AND_SEP).

make_sets(Sets) ->
  concat(make_key_values(Sets, [], ?EQU_SEP), [], ?COMMA_SEP).

make_key_values([{Key, Value}|Tail], Acc, Sep) when Value =:= null;
                                                    Value =:= "null";
                                                    Value =:= undefined;
                                                    Value =:= "undefined" ->
  make_key_values([{Key, Sep, null}|Tail], Acc, Sep);
make_key_values([{Key, Value}|Tail], Acc, Sep) ->
  Value_ = mysql:encode(Value),
  make_key_values(Tail, [make_flattent_list("~w = ~s", [Key, Value_])|Acc], Sep);
make_key_values([{Key, Operator, Value}|Tail], Acc, Sep) ->
  Value_ = mysql:encode(Value),
  make_key_values(Tail, [make_flattent_list("~w ~s ~s", [Key, Operator, Value_])|Acc], Sep);
make_key_values([], Acc, _) -> lists:reverse(Acc).

filter_null_and_undefined([{_, Value}|Tail], Acc) when Value =:= null;
                                                       Value =:= "null";
                                                       Value =:= undefined;
                                                       Value =:= "undefined" ->
  filter_null_and_undefined(Tail, Acc);
filter_null_and_undefined([Term|Tail], Acc) ->
  filter_null_and_undefined(Tail, [Term | Acc]);
filter_null_and_undefined([], Acc) -> lists:reverse(Acc).
