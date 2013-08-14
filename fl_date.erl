-module(fl_date).

-export([parse/1, days_in_month/2, is_end_of_month/1, test/0]).

-define(is_num(X), (X >= $0 andalso X =< $9)).

days_in_month(_, _) -> 0.

parse([N1, N2, N3, N4, N5, N6, N7, N8]) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4), ?is_num (N5), ?is_num (N6), ?is_num (N7), ?is_num (N8) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	Month = list_to_integer([N5, N6]),
	Day = list_to_integer([N7, N8]),
	validate(Year, Month, Day);
parse([N1, N2, Separator | Rest]) when ?is_num (N1), ?is_num (N2) ->
	DayOrMonth = list_to_integer([N1, N2]),
	parse(Rest, DayOrMonth, Separator);
parse([N, Separator | Rest]) when ?is_num (N) ->
	DayOrMonth = list_to_integer([N]),
	parse(Rest, DayOrMonth, Separator);
parse(Other) ->
	{bad_date, Other}.

validate(Year, Month, Day) when ((Month < 1) or (Month > 12)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when (Day < 1) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 1) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 2) andalso (Year rem 4 =:= 0) andalso (Day > 29)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 2) andalso (Year rem 4 =/= 0) andalso (Day > 28)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 3) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 4) andalso (Day > 30)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 5) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 6) andalso (Day > 30)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 7) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 8) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 9) andalso (Day > 30)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 10) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 11) andalso (Day > 30)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) when ((Month =:= 12) andalso (Day > 31)) ->
	{bad_date, {Year, Month, Day}};
validate(Year, Month, Day) ->
	{Year, Month, Day}.

parse([N1, N2, Separator, N3, N4, N5, N6], Month, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4), ?is_num (N5) , ?is_num (N6) ->
	Day = list_to_integer([N1, N2]),
	Year = list_to_integer([N3, N4, N5, N6]),
	validate(Year, Month, Day);
parse([N1, Separator, N2, N3, N4, N5], Month, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4), ?is_num (N5) ->
	Day = list_to_integer([N1]),
	Year = list_to_integer([N2, N3, N4, N5]),
	validate(Year, Month, Day);
parse("jan" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 1, Day);
parse("feb" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 2, Day);
parse("mar" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 3, Day);
parse("apr" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 4, Day);
parse("may" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 5, Day);
parse("jun" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 6, Day);
parse("jul" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 7, Day);
parse("aug" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 8, Day);
parse("sep" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 9, Day);
parse("oct" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 10, Day);
parse("nov" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 11, Day);
parse("dec" ++ [Separator, N1, N2, N3, N4], Day, Separator) when ?is_num (N1), ?is_num (N2), ?is_num (N3), ?is_num (N4) ->
	Year = list_to_integer([N1, N2, N3, N4]),
	validate(Year, 12, Day).

is_end_of_month({Year, Month, Day}) ->
	case calendar:last_day_of_the_month(Year, Month) of
		Day	-> true;
		_	-> false
	end.

test() ->
	fl_test:many([
		{"Parse",			fun test_parse/0},
		{"Days In Month",	fun test_days_in_month/0}
	]).

test_parse() ->
	Dates = ["20120208", "02/08/2012", "2/08/2012", "02/8/2012", "02-08-2012", "2-08-2012", "02-8-2012", "8-feb-2012", "33/08/2012", "00/08/2012", "02/0/2012", "02/29/2012", "02/29/2013", "02/30/2012"] ++ [integer_to_list(Month) ++ "/35/2012" || Month <- lists:seq(1, 12)],
	Format = "~12s~25s~n",
	io:format(Format, ["Input", "Output"]),
	[io:format(Format, [Date, io_lib:write(parse(Date))]) || Date <- Dates].

test_days_in_month() ->
	Years = [2012, 2013],
	Months = lists:seq(1,12),
	io:format("~5s~6s~5s~n", ["Year", "Month", "Days"]),
	[io:format("~5B~6B~5B~n", [Year, Month, calendar:last_day_of_the_month(Year, Month)]) || Year <- Years, Month <- Months].
