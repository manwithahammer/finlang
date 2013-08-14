% c('test.erl').

-module(fl_test).
-export([time_function/3, avg_time/4]).

time_function(Fun, N, M) ->
	NN = lists:seq(1, N),
	MM = lists:seq(1, M),
	G = fun() -> Fun() end,
	H = fun() -> [ timer:tc(G) || _X <- NN] end,
	Times = [lists:foldl(fun(X, Sum) -> {A, _} = X, A + Sum end, 0, H()) / N || _ <- MM],
	io:format("Average: ~p~n", [lists:foldl(fun(X, Sum) -> X + Sum end, 0, Times) / M]),
	io:format("Min: ~f~n", [lists:min(Times)]),
	io:format("Max: ~f~n", [lists:max(Times)])
	.

avg_time(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
	      "Median: ~b mics~n"
	      "Average: ~b mics~n",
	      [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).