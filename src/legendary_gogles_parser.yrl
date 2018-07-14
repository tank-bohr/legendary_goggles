Nonterminals root date time datetime.

Terminals integer time_separator date_separator month meridian_specifier
  date_marker time_marker now special_word shift date_unit weekday.

Rootsymbol root.

root -> date : '$1'.
root -> time : '$1'.
root -> datetime : '$1'.

date -> integer date_separator integer date_separator integer : legendary_gogles_rules:ymd('$1', '$3', '$5').
date -> integer month : legendary_gogles_rules:md('$2', '$1').
date -> month integer : legendary_gogles_rules:md('$1', '$2').
date -> special_word : legendary_gogles_rules:special_word('$1').
date -> shift date_unit : legendary_gogles_rules:shift_date_by_unit('$2', '$1').
date -> shift weekday : legendary_gogles_rules:shift_date_to_weekday('$2', '$1').
date -> date_marker date : '$2'.

time -> integer meridian_specifier : legendary_gogles_rules:hm('$1', {integer, 1, 0}, '$2').
time -> integer time_separator integer : legendary_gogles_rules:hm('$1', '$3').
time -> integer time_separator integer meridian_specifier : legendary_gogles_rules:hm('$1', '$3', '$4').
time -> time_marker time : '$2'.

datetime -> now : legendary_gogles_rules:now().
datetime -> time special_word : legendary_gogles_rules:time_and_special_word('$1', '$2').
datetime -> special_word time : legendary_gogles_rules:time_and_special_word('$2', '$1').
datetime -> date_marker date time_marker time : legendary_gogles_rules:date_time('$2', '$4').

Erlang code.
