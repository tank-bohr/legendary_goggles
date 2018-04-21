-module(legendary_goggles).

-export([
    parse/1
]).

parse(String) ->
    {ok, Tokens, _} = legendary_gogles_lexer:string(String),
    {ok, Time} = legendary_gogles_parser:parse(Tokens),
    Time.
