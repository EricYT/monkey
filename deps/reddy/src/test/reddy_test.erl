
-module(reddy_test).

-compile(export_all).


pressure(TotalPro, Count, Interval) ->
    Parent = self(),
    _Pids = [ erlang:spawn_link(?MODULE, loop_insert, [Parent, Count, Interval]) || Index <- lists:seq(1, TotalPro) ],
    receive_res(TotalPro),
    ok.

loop_insert(Parent, 0, _Interval) -> Parent ! {self(), complete};
loop_insert(Parent, N, Interval) ->
    reddy_strings:set('pool_put', "key"++integer_to_list(N), integer_to_list(N)),
    timer:sleep(Interval),
    loop_insert(Parent, N-1, Interval).

receive_res(0) -> ok;
receive_res(ChildrenCount) ->
    receive
        {_Child, complete} ->
            receive_res(ChildrenCount-1)
    end.
