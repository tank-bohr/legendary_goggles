-module(legendary_goggles_date_calc_test).
-include_lib("eunit/include/eunit.hrl").

shift_date_test_() ->
    Date = {2018, 7, 7},
    [?_assertEqual({2018, 7, 10}, legendary_goggles_date_calc:shift_date(Date, 3)),
    ?_assertEqual({2018, 7, 4}, legendary_goggles_date_calc:shift_date(Date, -3)),
    ?_assertEqual({2018, 7, 30}, legendary_goggles_date_calc:shift_date(Date, 23)),
    ?_assertEqual({2018, 8, 29}, legendary_goggles_date_calc:shift_date(Date, 53)),
    ?_assertEqual({2018, 12, 7}, legendary_goggles_date_calc:shift_date(Date, 153)),
    ?_assertEqual({2019, 3, 17}, legendary_goggles_date_calc:shift_date(Date, 253)),
    ?_assertEqual({2016, 12, 31}, legendary_goggles_date_calc:shift_date(Date, -553))].

shift_date_by_unit_test_() ->
    [?_assertEqual({2018, 1, 1}, legendary_goggles_date_calc:shift_date_by_unit({2017, 12, 31}, day, 1)),
    ?_assertEqual({2017, 12, 31}, legendary_goggles_date_calc:shift_date_by_unit({2018, 1, 1}, day, -1)),
    ?_assertEqual({2018, 7, 8}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, day, 1)),
    ?_assertEqual({2018, 7, 6}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, day, -1)),
    ?_assertEqual({2018, 7, 14}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, week, 1)),
    ?_assertEqual({2018, 6, 30}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, week, -1)),
    ?_assertEqual({2018, 8, 6}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, month, 1)),
    ?_assertEqual({2018, 6, 7}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, month, -1)),
    ?_assertEqual({2019, 7, 7}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, year, 1)),
    ?_assertEqual({2017, 7, 7}, legendary_goggles_date_calc:shift_date_by_unit({2018, 7, 7}, year, -1))].

shift_date_to_weekday_test_() ->
    [?_assertEqual({2018, 7, 9}, legendary_goggles_date_calc:shift_date_to_weekday({2018, 7, 7}, 1, 1)),
    ?_assertEqual({2018, 7, 4}, legendary_goggles_date_calc:shift_date_to_weekday({2018, 7, 7}, 3, 0)),
    ?_assertEqual({2018, 6, 29}, legendary_goggles_date_calc:shift_date_to_weekday({2018, 7, 7}, 5, -1))].
