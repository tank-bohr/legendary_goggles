-module(legendary_goggles_test).
-include_lib("eunit/include/eunit.hrl").

testee(Str) ->
    legendary_goggles:parse(Str).

parse_test_() ->
    [
        ?_assertEqual({23, 06}, testee("the time is 23:06")),
        ?_assertEqual({11, 1}, testee("11:01 am")),
        ?_assertEqual({23, 2}, testee("11 : 02 pm"))
    ].
