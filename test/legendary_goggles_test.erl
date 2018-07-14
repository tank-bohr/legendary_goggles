-module(legendary_goggles_test).
-include_lib("eunit/include/eunit.hrl").

testee(Str) ->
    legendary_goggles:parse(Str).

parse_test_() ->
    [
        ?_assertMatch({_, {23, 06}}, testee("time is 23:06")),
        ?_assertMatch({_, {11, 1}}, testee("11:01 am")),
        ?_assertMatch({_, {23, 2}}, testee("11 : 02 pm")),
        ?_assertMatch({_, {22, 0}}, testee("10pm")),
        ?_assertMatch({_, {15, 0}}, testee("at 3pm")),
        ?_assertMatch({{2018, 4, 22}, _}, testee("2018/04/22")),
        ?_assertMatch({{2018, 4, 22}, _}, testee("2018-04-22")),
        ?_assertMatch({{_, 1, 15}, _}, testee("Jan 15")),
        ?_assertMatch({{_, 4, 22}, _}, testee("22 April")),
        ?_assertMatch({{_, 6, 1}, _}, testee("on June 1st")),
        ?_assertEqual({{2018, 4, 22}, {8, 46}}, testee("on 2018-04-22 at 8:46"))
    ].

now_test() ->
    {Date, _Time} = testee("now"),
    ?assert(calendar:valid_date(Date)).

special_words_test_() ->
    Results = [testee(X) || X <- ["yesterday", "today", "tomorrow"]],
    [?_assert(calendar:valid_date(Date)) || {Date, _Time} <- Results].

next_week_test_() ->
    {NextWeek, _} = testee("next week"),
    {LastWeek, _} = testee("last week"),
    {Today, _} = calendar:universal_time(),
    %% ?debugVal(NextWeek),
    %% ?debugVal(LastWeek),
    [?_assert(calendar:valid_date(NextWeek)),
    ?_assert(calendar:valid_date(LastWeek)),
    ?_assert(NextWeek > Today),
    ?_assert(LastWeek < Today)].

shift_to_weekday_test_() ->
    {NextFriday, _} = testee("next Fri"),
    {LastMonday, _} = testee("last Monday"),
    {ThisWednesday, _} = testee("this Wed"),
    {Today, _} = calendar:universal_time(),
    %% ?debugVal(NextFriday),
    %% ?debugVal(LastMonday),
    [?_assert(calendar:valid_date(NextFriday)),
    ?_assert(calendar:valid_date(LastMonday)),
    ?_assert(calendar:valid_date(ThisWednesday)),
    ?_assertEqual(5, calendar:day_of_the_week(NextFriday)),
    ?_assertEqual(3, calendar:day_of_the_week(ThisWednesday)),
    ?_assertEqual(1, calendar:day_of_the_week(LastMonday)),
    ?_assertEqual(calendar:iso_week_number(), calendar:iso_week_number(ThisWednesday)),
    ?_assert(NextFriday > Today),
    ?_assert(LastMonday < Today)].

syntax_error_test() ->
    ?assertEqual(invalid, testee("at am to pm")).

special_word_plus_time_test_() ->
    {Date, Time} = testee("tomorrow at 4pm"),
    {Today, _} = calendar:universal_time(),
    [?_assertEqual({16, 0}, Time),
    ?_assert(Date > Today)].
