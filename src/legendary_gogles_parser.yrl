Nonterminals root date time datetime.

Terminals integer time_separator date_separator month meridian_specifier
  date_marker time_marker.

Rootsymbol root.

root -> date : '$1'.
root -> time : '$1'.
root -> datetime : '$1'.

date -> integer date_separator integer date_separator integer : ymd('$1', '$3', '$5').
date -> integer month : md('$2', '$1').
date -> month integer : md('$1', '$2').

time -> integer time_separator integer : hm('$1', '$3').
time -> integer time_separator integer meridian_specifier : hm('$1', '$3', '$4').

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

hm(_,_) ->
    invalid.

hm(Hour, Min, {meridian_specifier, _, am}) ->
    case hm(Hour, Min) of
      {H, M} when H =< 11 -> {H,M};
      _ -> invalid
    end;

hm(Hour, Min, {meridian_specifier, _, pm}) ->
    case hm(Hour, Min) of
      {H, M} when H >= 12 -> {H,M};
      {H, M} -> {H + 12, M}
    end.
