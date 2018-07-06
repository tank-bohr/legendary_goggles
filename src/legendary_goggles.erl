-module(legendary_goggles).

-export([
    parse/1
]).

-type minute() :: 0..59.
-type hour() :: 0..23.
-type time() :: {hour(), minute()}.
-type date() :: calendar:date().
-type datetime() :: {date(), time()}.

-export_type([
    minute/0,
    hour/0,
    time/0,
    date/0,
    datetime/0
]).

-spec parse(string()) -> date() | time() | datetime() | invalid.
parse(String) ->
    {ok, Tokens, _} = legendary_gogles_lexer:string(String),
    case legendary_gogles_parser:parse(Tokens) of
        {ok, Time} -> Time;
        _ -> invalid
    end.
