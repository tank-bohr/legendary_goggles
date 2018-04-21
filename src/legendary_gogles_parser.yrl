Nonterminals time.

Terminals integer time_separator meridian_specifier.

Rootsymbol time.

time -> integer time_separator integer : hm('$1', '$3').

time -> integer time_separator integer meridian_specifier : hm('$1', '$3', '$4').

Erlang code.

hm({integer, _, Hour}, {integer, _, Min})
    when Hour >= 0, Hour =< 23, Min >= 0, Min =< 59 ->
        {Hour, Min};

hm(_,_) ->
    notime.

hm(Hour, Min, {meridian_specifier, _, am}) ->
    case hm(Hour, Min) of
      {H, M} when H =< 11 -> {H,M};
      _ -> notime
    end;

hm(Hour, Min, {meridian_specifier, _, pm}) ->
    case hm(Hour, Min) of
      {H, M} when H >= 12 -> {H,M};
      {H, M} -> {H + 12, M}
    end.
