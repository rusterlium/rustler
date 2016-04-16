Nonterminals
root kv_key kv_pair
array array_elems
items item value
table_header table_header_body
inline_table inline_table_elems.

Terminals
key_bare lit_string_basic lit_string_basic_multi lit_string_literal lit_string_literal_multi
lit_integer lit_float lit_datetime1 lit_datetime2 lit_datetime3
'=' '[' ']' '{' '}' '.' ','.

Rootsymbol root.

root ->
    items : '$1'.

items ->
    item : ['$1'].
items ->
    item items : ['$1'] ++ '$2'.

item ->
    table_header : '$1'.
item ->
    kv_pair : '$1'.

table_header ->
    '[' '[' table_header_body ']' ']' : {table_array, '$3'}.
table_header ->
    '[' table_header_body ']' : {table, '$2'}.

table_header_body ->
    kv_key '.' table_header_body : ['$1'] ++ '$3'.
table_header_body ->
    kv_key : ['$1'].

kv_pair ->
    kv_key '=' value : {kv, '$1', '$3'}.

kv_key ->
    key_bare : {bare, extract_token('$1')}.
kv_key ->
    lit_string_basic : {basic, extract_token('$1')}.
kv_key ->
    lit_string_literal : {literal, extract_token('$1')}.

array ->
    '[' array_elems ']' : {array, '$2'}.

array_elems ->
    value ',' array_elems : ['$1'] ++ '3'.
array_elems ->
    value : ['$1'].

inline_table ->
    '{' inline_table_elems '}' : {inline_table, '$2'}.

inline_table_elems ->
    kv_pair ',' inline_table_elems : ['$1'] ++ inline_table_elems.
inline_table_elems ->
    kv_pair : ['$1'].

value ->
    array : '$1'.
value ->
    inline_table : '$1'.

value ->
    lit_string_basic : {val_basic, extract_token('$1')}.
value ->
    lit_string_basic_multi : {val_basic_multi, extract_token('$1')}.
value ->
    lit_string_literal : {val_literal, extract_token('$1')}.
value ->
    lit_string_literal_multi : {val_literal_multi, extract_token('$1')}.

value ->
    lit_integer : {val_integer, extract_token('$1')}.
value ->
    lit_float : {val_float, extract_token('$1')}.

value ->
    lit_datetime1 : {val_datetime1, extract_token('$1')}.
value ->
    lit_datetime2 : {val_datetime2, extract_token('$1')}.
value ->
    lit_datetime3 : {val_datetime3, extract_token('$1')}.

Erlang code.

to_str(V) ->
    'Elixir.String.Chars':to_string(V).

extract_token({_Token, _Line, Value}) -> to_str(Value).
