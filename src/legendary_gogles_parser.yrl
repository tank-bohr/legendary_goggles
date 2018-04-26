Nonterminals root date time datetime.

Terminals integer time_separator date_separator month meridian_specifier
  date_marker time_marker now special_word shift date_unit weekday.

Rootsymbol root.

root -> date : '$1'.
root -> time : '$1'.
root -> datetime : '$1'.

date -> integer date_separator integer date_separator integer : ymd('$1', '$3', '$5').
date -> integer month : md('$2', '$1').
date -> month integer : md('$1', '$2').
date -> special_word : special_word('$1').
date -> shift date_unit : shift_date_by_unit('$2', '$1').
date -> shift weekday : shift_date_to_weekday('$2', '$1').
date -> date_marker date : '$2'.

time -> integer meridian_specifier : hm('$1', {integer, 1, 0}, '$2').
time -> integer time_separator integer : hm('$1', '$3').
time -> integer time_separator integer meridian_specifier : hm('$1', '$3', '$4').
time -> time_marker time : '$2'.

datetime -> now : date_hm().
datetime -> time special_word : time_and_special_word('$1', '$2').
datetime -> special_word time : time_and_special_word('$2', '$1').
datetime -> date_marker date time_marker time : {'$2', '$4'}.

Erlang code.

ymd({integer, _, Year}, {integer, _, Month}, {integer, _, Day}) ->
    Date = {Year, Month, Day},
    ValidDate = calendar:valid_date(Date),
    case ValidDate of
        true -> Date;
        fale -> invalid
    end.

md({month, _, Month}, {integer, _, Day}) ->
    {{Year, _, _}, _Time} = calendar:universal_time(),
    Date = {Year, Month, Day},
    ValidDate = calendar:valid_date(Date),
    case ValidDate of
        true -> Date;
        fale -> invalid
    end.

hm({integer, _, Hour}, {integer, _, Min})
    when Hour >= 0, Hour =< 23, Min >= 0, Min =< 59 ->
        {Hour, Min};
hm(_, _) ->
    invalid.

hm(Hour, Min, {meridian_specifier, _, am}) ->
    case hm(Hour, Min) of
      {H, M} when H =< 11 -> {H, M};
      _ -> invalid
    end;

hm(Hour, Min, {meridian_specifier, _, pm}) ->
    case hm(Hour, Min) of
      {H, M} when H >= 12 -> {H, M};
      {H, M} -> {H + 12, M}
    end.

date_hm() ->
    {Date, {H, M, _}} = calendar:universal_time(),
    {Date, {H, M}}.

special_word({special_word, _, SpecialWord}) ->
    {BaseDate, _Time} = calendar:universal_time(),
    case SpecialWord of
        today -> BaseDate;
        tomorrow -> shift_date(BaseDate, 1);
        yesterday -> shift_date(BaseDate, -1)
    end.

time_and_special_word(Time, SpecialWord) ->
    Date = special_word(SpecialWord),
    {Date, Time}.

shift_date_by_unit({date_unit, _, Unit}, {shift, _, Direction}) ->
    {BaseDate, _Time} = calendar:universal_time(),
    shift_date_by_unit(BaseDate, Unit, Direction).

shift_date_by_unit(BaseDate, _Unit, 0) ->
    BaseDate;
shift_date_by_unit(BaseDate, day, Direction) ->
    shift_date(BaseDate, Direction);
shift_date_by_unit(BaseDate, week, Direction) ->
    shift_date(BaseDate, 7 * Direction);
shift_date_by_unit(BaseDate, month, Direction) ->
    %% Naive implementaion
    shift_date(BaseDate, 30 * Direction);
shift_date_by_unit({Y, M, D}, year, Direction) ->
    Year = Y + Direction,
    case { calendar:is_leap_year(Year), M, D} of
        {false, 2, 29} -> {Year, 2, 28};
        {true, _, _} -> {Year, M, D}
    end.

shift_date_to_weekday({weekday, _, Weekday}, {shift, _, Direction}) ->
    {BaseDate, _Time} = calendar:universal_time(),
    BaseWeekday = calendar:day_of_the_week(BaseDate),
    case Direction of
        1 ->
            WeekCompletion = 7 - BaseWeekday,
            shift_date(BaseDate, WeekCompletion + Weekday);
        0 ->
            shift_date(BaseDate, Weekday - BaseWeekday);
        -1 ->
            WeekCompletion = 7 - Weekday,
            shift_date(BaseDate, - BaseWeekday - WeekCompletion)
    end.


shift_date(Date, Shift) ->
    calendar:gregorian_days_to_date(
        calendar:date_to_gregorian_days(Date) + Shift).
