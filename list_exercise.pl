my_len([], 0).
my_len([_|T], L1) :-
    my_len(T, L2),
    L1 is 1 + L2.

my_last([H|[]], H).
my_last([_|T], E) :-
    my_last(T, E).

my_split([H|T], [H], T, H).
my_split([H|T], [H|L1], L2, E) :-
    my_split(T, L1, L2, E).

my_after([F, S|_], F, S).
my_after([_|T], E, A) :-
    my_after(T, E, A).

my_remove([H|T], T, H).
my_remove([H|T], [H|R], E) :-
    my_remove(T, R, E).

my_max([H|[]], H).
my_max([H|T], MAX) :-
    my_max(T, M),
    ((H >= M, MAX = H);
    (M >= H, MAX = M)),
    !.