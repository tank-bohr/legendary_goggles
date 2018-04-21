Definitions.

D = [0-9]
L = [a-zA-Z]
WS = [\s\t]
AM = am|AM|a\.m\.|A\.M\.|a
PM = pm|PM|p\.m\.|P\.M\.|p
COLON = :

Rules.

{D}+    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{COLON} : {token, {time_separator, TokenLine}}.
{AM}    : {token, {meridian_specifier, TokenLine, am}}.
{PM}    : {token, {meridian_specifier, TokenLine, pm}}.
{WS}+   : skip_token.
{L}+    : skip_token.

Erlang code.
