%%%----------------------------------------------------------------------
%%%
%%% wg @copyright 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the crontab parse module (simlar with *unix crontab, please read
%%%      crontab.
%%%
%%%----------------------------------------------------------------------
-module(ecrontab_parse).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("ecrontab.hrl").

-export([parse/1, parse_field/3]).

%% @doc parse the crontab config file
-spec parse(File :: string()) ->
    {ok, [cron_entry()]} | {'error', any()}.
parse(File) ->
    case file:consult(File) of
        {error, enoent} = Error ->
            ?Warn2("crontab file ~p not exist~n", [File]),
            Error;
        {error, R} = Error ->
            ?Warn2("crontab file error: ~p~n", [file:format_error(R)]),
            Error;
        {ok, CronTab} ->
            parse_entrys(CronTab)
    end.

%%-----------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------

%% parse all the entrys
parse_entrys(CronTab) ->
    Entrys =
    lists:foldl(
        fun(Entry, Acc) ->
                case catch parse_entry(Entry) of
                    {ok, CronEntry} ->
                        [CronEntry | Acc];
                    {error, R} ->
                        ?Warn2("the line :~p error:~p~n", [Entry, R]),
                        Acc
                end
        end,
        [],
        CronTab),
    {ok, Entrys}.


%% parse the single entry
parse_entry({{M, H, Dom, Mon, Dow}, {Mod, F, A} = MFA}) when is_atom(Mod), is_atom(F), is_list(A) ->
    Cron =
    #cron_entry{
        m = parse_field(M, 0, 59, emin),
        h = parse_field(H, 0, 23, ehour),
        dom = parse_field(Dom, 1, 31, edom),
        mon = parse_field(Mon, 1, 12, emon),
        dow = parse_field(Dow, 0, 7, edow),
        mfa = MFA
    },
    {ok, Cron}; 
parse_entry(_) ->
    {error, eformat}.


%% parset the fileld
parse_field(F, Min, Max, Error) ->
    try parse_field(F, Min, Max) of
        Result when is_record(Result,cron_field) -> Result;
        Reason -> throw({error, {Error,Reason}})
    catch _:Reason ->
        throw({error,{Error,Reason}})
    end.


parse_field("*", Min, Max) ->
    #cron_field{type = ?CRON_RANGE, value = {Min, Max, 1}};
parse_field(F = [_|_], Min, Max) when is_list(F) ->
    case catch list_to_integer(F) of
        V when is_integer(V) -> parse_field(V,Min,Max);
        _ ->
            case string:tokens(F, ",") of
                [Single] -> % is range
                    case parse_range(Single) of
                        {First, Last, _Step} = Range when First >= Min, Last =< Max ->
                            #cron_field{type = ?CRON_RANGE, value = Range}
                    end;
                [_|_] = Multi -> % is list
                    #cron_field{type = ?CRON_LIST, value = lists:map(fun(E) -> parse_field(E, Min, Max) end,Multi)}
            end
    end;
parse_field(F, Min, Max) when F >= Min, F =< Max ->
    #cron_field{type = ?CRON_NUM, value = F}.

%% parse the range string: "2-5/2", "2-5"
parse_range(Str) ->
    {RangeStr, Step} = 
    case string:tokens(Str, "/") of
        [Range] ->
            {Range, 1};
        [Range, StepStr] ->
            {Range, list_to_integer(StepStr)}
    end,
    [First, Last] = string:tokens(RangeStr, "-"),
    {list_to_integer(First), list_to_integer(Last), Step}.
