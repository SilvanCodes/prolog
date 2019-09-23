:- use_module(library(dcg/basics)).

main :-
    phrase_from_file(object, 'test.json').

object -->
    curly_paren_open,
    kvs,
    curly_paren_close.

curly_paren_open -->
   blanks,
    "{",
    !,
    blanks.

curly_paren_close -->
    blanks,
    "}",
    !,
    blanks.

kvs -->
    [].
kvs -->
    kv.
kvs -->
    kv,
    blanks,
    ",",
    !,
    blanks,
    kvs.

kv -->
    key,
    blanks,
    ":",
    blanks,
    value.

key -->
    string.

value -->
    boolean.
value -->
    string.
value -->
    digits.
value -->
    float.
value -->
    list.
value -->
    object.

values -->
    [].
values -->
    value.
values -->
    value,
    blanks,
    ",",
    blanks,
    values.

boolean -->
    "true";
    "false".

list -->
    "[",
    blanks,
    values,
    blanks,
    "]".

string -->
    "\"",
    alnums,
    "\"".

float -->
    digits,
    ".",
    digits. 

alnums -->
    [].
alnums -->
    [X],
    alnums,
    { is_alnum(X) }.

digits -->
    [X|[]],
    { is_digit(X) }.
digits -->
    [X],
    digits,
    { is_digit(X) }.
