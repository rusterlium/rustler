Definitions.

STRING_BASIC = \"([^\"\\]|\\.)*\"
STRING_BASIC_MULTI = \"{3}.*\"{3}
STRING_LITERAL = \'[^\']*\'
STRING_LITERAL_MULTI = \'{3}.*\'{3}

INTEGER = [\+-]?[0-9_]+
FLOAT = [\+-]?[0-9_]+(\.[0-9_]+)+([Ee][\+-]?[0-9_]+)?
BARE_KEY = [A-Za-z0-9-_]+

BOOLEAN = (true|false)

DATETIME1 = \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{1,3})?(([\+\-]\d{2}:\d{2})|Z)
DATETIME2 = \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{1,3})?
DATETIME3 = \d{4}-\d{2}-\d{2}

COMMENT = #.+?([\n\r])
WHITESPACE = \s
NEWLINE = (\n|\r\n)

Rules.

{COMMENT} : skip_token.

{STRING_BASIC} : {token, {lit_string_basic, TokenLine, TokenChars}}.
{STRING_BASIC_MULTI} : {token, {lit_string_basic_multi, TokenLine, TokenChars}}.
{STRING_LITERAL} : {token, {lit_string_literal, TokenLine, TokenChars}}.
{STRING_LITERAL_MULTI} : {token, {lit_string_literal_multi, TokenLine, TokenChars}}.

{INTEGER} : {token, {lit_integer, TokenLine, TokenChars}}.
{FLOAT} : {token, {lit_float, TokenLine, TokenChars}}.
{BARE_KEY} : {token, {key_bare, TokenLine, TokenChars}}.

{DATETIME1} : {token, {lit_datetime1, TokenLine, TokenChars}}.
{DATETIME2} : {token, {lit_datetime2, TokenLine, TokenChars}}.
{DATETIME3} : {token, {lit_datetime3, TokenLine, TokenChars}}.

= : {token, {'=', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\. : {token, {'.', TokenLine}}.
\, : {token, {',', TokenLine}}.


{WHITESPACE}+ : skip_token.
{NEWLINE}+ : skip_token.

Erlang code.
