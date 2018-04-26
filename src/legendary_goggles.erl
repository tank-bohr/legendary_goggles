-module(legendary_goggles).

-export([
    parse/1
]).

-type date() :: calendar:date().
-type hour() :: 0..23.
-type minute() :: 0..59.
-type time() :: {hour(), minute()}.
-type datetime() :: {date(), time()}.

-spec parse(string()) -> date() | time() | datetime() | invalid.
parse(String) ->
    {ok, Tokens, _} = legendary_gogles_lexer:string(String),
    case legendary_gogles_parser:parse(Tokens) of
        {ok, Time} -> Time;
        _ -> invalid
    end.
