-module(legendary_goggles_test).
-include_lib("eunit/include/eunit.hrl").

testee(Str) ->
    legendary_goggles:parse(Str).

parse_test_() ->
    [
        ?_assertEqual({23, 06}, testee("time is 23:06")),
        ?_assertEqual({11, 1}, testee("11:01 am")),
        ?_assertEqual({23, 2}, testee("11 : 02 pm")),
        ?_assertEqual({2018, 4, 22}, testee("2018/04/22")),
        ?_assertEqual({2018, 4, 22}, testee("2018-04-22")),
        ?_assertMatch({_, 1, 15}, testee("Jan 15")),
        ?_assertMatch({_, 4, 22}, testee("22 April")),
        ?_assertEqual({{2018, 4, 22}, {8, 46}}, testee("on 2018-04-22 at 8:46"))
    ].

now_test() ->
    {Date, _Time} = testee("now"),
    ?assert(calendar:valid_date(Date)).

special_words_test_() ->
    [?_assert(calendar:valid_date(testee(X)))
        || X <- ["yesterday", "today", "tomorrow"]].

next_week_test_() ->
    NextWeek = testee("next week"),
    LastWeek = testee("last week"),
    [?_assert(calendar:iso_week_number(NextWeek) > calendar:iso_week_number()),
    ?_assert(calendar:iso_week_number(LastWeek) < calendar:iso_week_number())].
