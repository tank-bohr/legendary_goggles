-module(legendary_gogles_rules).

-export([
    ymd/3,
    md/2,
    hm/2,
    hm/3,
    now/0,
    date_time/2,
    special_word/1,
    time_and_special_word/2,
    shift_date_by_unit/2,
    shift_date_to_weekday/2
]).

ymd({integer, _, Year}, {integer, _, Month}, {integer, _, Day}) ->
    case calendar:valid_date({Year, Month, Day}) of
        true ->
            make_fun(#{
                year  => Year,
                month => Month,
                day   => Day
            });
        false ->
            invalid
    end.

md({month, _, Month}, {integer, _, Day}) ->
    case legendary_goggles_date_calc:is_valid_date(Month, Day) of
        true ->
            make_fun(#{
                month => Month,
                day   => Day
            });
        false ->
            invalid
    end.

hm({integer, _, Hour}, {integer, _, Min}) ->
    case legendary_goggles_date_calc:is_valid_time(Hour, Min) of
        true ->
            make_fun(#{
                hour   => Hour,
                minute => Min
            });
        false ->
            invalid
    end.

hm({integer, _, Hour} = H, M, {meridian_specifier, _, am}) when Hour =< 11 ->
    hm(H, M);
hm({integer, _, Hour} = H, M, {meridian_specifier, _, pm}) when Hour =< 11 ->
    hm(setelement(3, H, Hour + 12), M);
hm(_H, _M, _S) ->
    invalid.

now() ->
    fun (_BaseDate, _DefaultTime) ->
        {Date, {H, M, _}} = calendar:universal_time(),
        {Date, {H, M}}
    end.

date_time(DateFun, TimeFun) ->
    fun (BaseDate, DefaultTime) ->
        {Date, _} = DateFun(BaseDate, DefaultTime),
        {_, Time} = TimeFun(BaseDate, DefaultTime),
        {Date, Time}
    end.

special_word({special_word, _, SpecialWord}) ->
    case SpecialWord of
        today ->
            fun (BaseDate, DefaultTime) ->
                {BaseDate, DefaultTime}
            end;
        tomorrow ->
            fun (BaseDate, DefaultTime) ->
                {legendary_goggles_date_calc:shift_date(BaseDate, 1), DefaultTime}
            end;
        yesterday ->
            fun (BaseDate, DefaultTime) ->
                {legendary_goggles_date_calc:shift_date(BaseDate, -1), DefaultTime}
            end
    end.

time_and_special_word(TimeFun, SpecialWord) ->
    F = special_word(SpecialWord),
    fun (BaseDate, DefaultTime) ->
        {_, Time} = TimeFun(BaseDate, DefaultTime),
        setelement(2, F(BaseDate, DefaultTime), Time)
    end.

shift_date_by_unit({date_unit, _, Unit}, {shift, _, Direction}) ->
    fun (BaseDate, DefaultTime) ->
        {legendary_goggles_date_calc:shift_date_by_unit(BaseDate, Unit, Direction), DefaultTime}
    end.

shift_date_to_weekday({weekday, _, Weekday}, {shift, _, Direction}) ->
    fun (BaseDate, DefaultTime) ->
        {legendary_goggles_date_calc:shift_date_to_weekday(BaseDate, Weekday, Direction), DefaultTime}
    end.

make_fun(Map) when is_map(Map) ->
    fun({Y, M, D}, {H, Min}) ->
        #{year := Year, month := Month, day := Day, hour := Hour, minute := Minute} =
            maps:merge(#{year => Y, month => M, day => D, hour => H, minute => Min}, Map),
        {{Year, Month, Day}, {Hour, Minute}}
    end.
