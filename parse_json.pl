:- use_module(library(dcg/basics)).

main :-
    phrase_from_file(object, 'test.json').

% object --> curly_paren_open, kv, blanks, ",", blanks, kv, curly_paren_close.
object --> curly_paren_open, kv, curly_paren_close.

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
    kv,
    !,
    blanks,
    ",",
    !,
    blanks,
    kv,
    !.
% kvs --> kv.
% kvs --> [].

kv -->
    "\"",
    alnums,
    "\"",
    blanks,
    ":",
    blanks,
    "\"",
    floats,
    "\"",
    !.

floats -->
    [X],
    { float(X) }.
value -->
    alnums.

alnums -->
    [X],
    alnums,
    { is_alnum(X) }.
alnums -->
    [].
