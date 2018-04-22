-module(legendary_goggles_test).
-include_lib("eunit/include/eunit.hrl").

testee(Str) ->
    legendary_goggles:parse(Str).

parse_test_() ->
    [
        ?_assertEqual({23, 06}, testee("the time is 23:06")),
        ?_assertEqual({11, 1}, testee("11:01 am")),
        ?_assertEqual({23, 2}, testee("11 : 02 pm")),
        ?_assertEqual({2018, 4, 22}, testee("2018/04/22")),
        ?_assertEqual({2018, 4, 22}, testee("2018-04-22")),
        ?_assertMatch({_, 1, 15}, testee("Jan 15")),
        ?_assertMatch({_, 4, 22}, testee("22 April")),
        ?_assertEqual({{2018, 4, 22}, {8, 46}}, testee("on 2018-04-22 at 8:46"))
    ].
