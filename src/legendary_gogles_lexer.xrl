Definitions.

D = [0-9]
L = [a-zA-Z]
WS = [\s\t]
AM = am|AM|a\.m\.|A\.M\.|a
PM = pm|PM|p\.m\.|P\.M\.|p
COLON = :
MONTH = Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?
WEEKDAY = Sun(day)?|Mon(day)?|Tue(sday)?|Wed(nesday)?|Thu(rsday)?|Fri(day)?|Sat(urday)?|weekend
DATE_UNIT = days?|weeks?|months?|years?
NUMERAL = one|first|two|second|three|third|four(th)?|five|fifth|six(th)?|seven(th)?|eighth?|nine(th)?|ten(th)?
SPECIAL_WORD = today|tomorrow|yesterday

Rules.

{D}+           : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{COLON}        : {token, {time_separator, TokenLine}}.
[\/\-]         : {token, {date_separator, TokenLine}}.
{AM}           : {token, {meridian_specifier, TokenLine, am}}.
{PM}           : {token, {meridian_specifier, TokenLine, pm}}.
{DATE_UNIT}    : {token, {date_unit, TokenLine, unit(TokenChars)}}.
{MONTH}        : {token, {month, TokenLine, month(TokenChars)}}.
{WEEKDAY}      : {token, {weekday, TokenLine, weekday(TokenChars)}}.
on             : {token, {date_marker, TokenLine}}.
at             : {token, {time_marker, TokenLine}}.
now            : {token, {now, TokenLine, now}}.
{SPECIAL_WORD} : {token, {special_word, TokenLine, list_to_atom(TokenChars)}}.
last           : {token, {shift, TokenLine, -1}}.
this           : {token, {shift, TokenLine, 0}}.
next           : {token, {shift, TokenLine, 1}}.
{WS}+          : skip_token.
{L}+           : skip_token.

Erlang code.

unit(Unit) ->
    list_to_atom(singularize(Unit)).

singularize(Str) ->
    string:trim(Str, trailing, "s").

month(Month) ->
    case lists:sublist(Month, 3) of
        "Jan" -> 1;
        "Feb" -> 2;
        "Mar" -> 3;
        "Apr" -> 4;
        "May" -> 5;
        "Jun" -> 6;
        "Jul" -> 7;
        "Aug" -> 8;
        "Sep" -> 9;
        "Oct" -> 10;
        "Nov" -> 11;
        "Dec" -> 12
    end.

weekday("weekend") ->
    6;
weekday(Str) ->
    case lists:sublist(Str, 3) of
        "Mon" -> 1;
        "Tue" -> 2;
        "Wed" -> 3;
        "Thu" -> 4;
        "Fri" -> 5;
        "Sat" -> 6;
        "Sun" -> 7
    end.
