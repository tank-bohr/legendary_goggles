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
        false -> invalid
    end.

md({month, _, Month}, {integer, _, Day}) ->
    {{Year, _, _}, _Time} = calendar:universal_time(),
    Date = {Year, Month, Day},
    ValidDate = calendar:valid_date(Date),
    case ValidDate of
        true -> Date;
        false -> invalid
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
        tomorrow -> legendary_goggles_date_calc:shift_date(BaseDate, 1);
        yesterday -> legendary_goggles_date_calc:shift_date(BaseDate, -1)
    end.

time_and_special_word(Time, SpecialWord) ->
    Date = special_word(SpecialWord),
    {Date, Time}.

shift_date_by_unit({date_unit, _, Unit}, {shift, _, Direction}) ->
    {BaseDate, _Time} = calendar:universal_time(),
    legendary_goggles_date_calc:shift_date_by_unit(BaseDate, Unit, Direction).

shift_date_to_weekday({weekday, _, Weekday}, {shift, _, Direction}) ->
    {BaseDate, _Time} = calendar:universal_time(),
    legendary_goggles_date_calc:shift_date_to_weekday(BaseDate, Weekday, Direction).
