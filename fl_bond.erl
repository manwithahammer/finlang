-module(fl_bond).

-export([bond_price/3, bond_yield/3, test/0]).

-define(tolerance, 0.000000001).

value_cashflow(CashFlow, Today, Yield) ->
  {Amount, Time} = CashFlow,
	Amount / (1.0 + Yield * (Time - Today)).

bond_price(CashFlows, Yield, Today) ->
	lists:foldl(fun(CashFlow, Sum) -> Sum + value_cashflow(CashFlow, Today, Yield) end, 0.0, CashFlows).

bond_price_bisection(CashFlows, Price, Time, Yleft, Yright, Iteration) when Iteration < 100 ->
	Ymid = 0.5 * (Yleft + Yright),
	Pmid = bond_price(CashFlows, Ymid, Time),
	% io:format("Left = ~p, Right = ~p, Pmid = ~p, Diff = ~p~n", [Yleft, Yright, Pmid, abs(Pmid - Price)]),
	if
		abs(Pmid - Price) < ?tolerance ->
			Ymid;
		Pmid < Price ->
			bond_price_bisection(CashFlows, Price, Time, Yleft, Ymid, Iteration + 1);
		true ->
			bond_price_bisection(CashFlows, Price, Time, Ymid, Yright, Iteration + 1)
	end;
bond_price_bisection(CashFlows, Price, Time, _Yleft, _Yright, _Iteration) ->
	{bond_price_bisection_iterations_exceeded, {cashflows, CashFlows}, {price, Price}, {time, Time}}.

bond_yield(CashFlows, Price, Time) ->
	{Yleft, Yright} = bond_bracket_price(CashFlows, Price, Time, 0.01, 0.02, 0),
	bond_price_bisection(CashFlows, Price, Time, Yleft, Yright, 0).

bond_bracket_price(CashFlows, Price, Time, Yleft, Yright, Iteration) when Iteration < 100->
	Pleft = bond_price(CashFlows, Yleft, Time),
	if
		Pleft < Price ->
			% io:format("[Left/~p/~p]~n", [Yleft, Pleft]),
			bond_bracket_price(CashFlows, Price, Time, Yleft - 0.001, Yright, Iteration + 1);
		true ->
			Pright = bond_price(CashFlows, Yright, Time),
			if
				Pright > Price ->
					% io:format("[Right/~p/~p]~n", [Yright, Pright]),
					bond_bracket_price(CashFlows, Price, Time, Yleft, Yright + 0.001, Iteration + 1);
				true ->
					% io:format("Price, Left = ~p, Right = ~p~n", [Pleft, Pright]),
					{Yleft, Yright}
			end
	end;
bond_bracket_price(CashFlows, Price, Time, _Yleft, _Yright, _Iteration) ->
	{bond_bracket_price_iterations_exceeded, {cashflows, CashFlows}, {price, Price}, {time, Time}}.

test() ->
	fl_test:many([
		{"Price",			fun test_price/0},
		{"Yield",			fun test_yield/0},
		{"Consistency",		fun test_consistency/0}
	]).

format_output({Label, Value}) ->
	format_output({Label, Value}, 20);
format_output([{Label, Value} | Tail]) ->
	format_output([{Label, Value} | Tail], 20).

format_output({Label, Value}, Width) ->
	io:format("~-" ++ integer_to_list(Width) ++ "s: ~p~n", [Label, Value]);
format_output([{Label, Value} | Tail], Width) ->
	format_output({Label, Value}, Width),
	format_output(Tail, Width);
format_output([], _Width) ->
	ok.

test_price() ->
	CashFlows = [{2.5, 0.5}, {2.5, 1.0}, {100.0, 1.0}],
	Yield = 0.02,
	Time = 0.0,
	Price = bond_price(CashFlows, Yield, Time),
	format_output([{"CashFlows", CashFlows}, {"Yield", Yield}, {"Price", Price}]).

test_yield() ->
	CashFlows = [{1.5, 0.5}, {1.5, 1.0}, {100.0, 1.0}],
	Price = 102.0,
	Time = 0.0,
	Yield = bond_yield(CashFlows, Price, Time),
	format_output([{"CashFlows", CashFlows}, {"Price", Price}, {"Yield", Yield}]).

test_consistency() ->
	CashFlows = [{1.5, 0.5}, {1.5, 1.0}, {100.0, 1.0}],
	Price = 102.0,
	Time = 0.0,
	Yield = bond_yield(CashFlows, Price, Time),
	Reprice = bond_price(CashFlows, Yield, Time),
	format_output([{"CashFlows", CashFlows}, {"Price", Price}, {"Yield:", Yield}, {"Reprice", Reprice},
		{"Result", case abs(Price - Reprice) < ?tolerance of true -> "Success"; false -> "Error" end}]).
