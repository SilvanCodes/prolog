activity(a, 4, [], 210).
activity(b, 3, [a], 400).
activity(c, 7, [b], 500).
activity(d, 8, [b], 540).
activity(e, 6, [b], 540).
activity(f, 6, [c], 500).
activity(g, 6, [d], 150).
activity(h, 8, [d,e], 600).
activity(i, 7, [f,g], 600).
activity(j, 3, [h,i], 600).
activity(k, 1, [j], 600).


vkn :- write_vkn.

write_vkn :-
    activity(A, _, _, _),
    desc(A, (FA/FE)/(SA/SE), TB/FB),
    respond(['Activity ', A]),
    tab(2),
    respond(['FA: ', FA, '\tFE: ', FE]),
    tab(2),
    respond(['SA: ', SA, '\tSE: ', SE]),
    tab(2),
    respond(['GP: ', TB, '\tFP: ', FB]),
    fail.
write_vkn.

critical_path([]) :-
    activity(A, _, _, _),
    buffer(A, 0/(0)).

respond([]) :-
    nl.
respond([H|T]) :-
    write(H),
    respond(T).

desc(A, (FA/FE)/(SA/SE), TB/FB) :-
    cross(A, (FA/FE)/(SA/SE)),
    buffer(A, TB/FB).

cross(A, (FA/FE)/(SA/SE)) :-
    find_FA_FE(A, FA/FE),
    find_SA_SE(A, SA/SE).

buffer(A, TB/FB) :-
    successors(A, S),
    cross(A, (FA/FE)/(SA/_)),
    min_FA(S, MFA),
    TB is SA - FA,
    FB is MFA - FE,
    !.
buffer(A, TB/(0)) :-
    successors(A, []),
    cross(A, (FA/_)/(SA/_)),
    TB is SA - FA.

min_FA([A|[]], FA) :-
    find_FA_FE(A, FA/_),
    !.
min_FA([A|T], MFA) :-
    find_FA_FE(A, FA1/_),
    min_SA(T, FA2),
    ((FA1 =< FA2, MFA = FA1);
    (FA2 =< FA1, MFA = FA2)).

find_SA_SE(A, SA/SE) :-
    activity(A, T, _, _),
    successors(A, []),
    findall(X, activity(X, _, _, _), L),
    max_FE(L, SE),
    SA is SE - T,
    !.
find_SA_SE(A, SA/SE) :-
    activity(A, T, _, _),
    successors(A, S),
    min_SA(S, SE),
    SA is SE - T.

min_SA([A|[]], SA) :-
    find_SA_SE(A, SA/_),
    !.
min_SA([A|T], MSA) :-
    find_SA_SE(A, SA1/_),
    min_SA(T, SA2),
    ((SA1 =< SA2, MSA = SA1);
    (SA2 =< SA1, MSA = SA2)).


find_FA_FE(A, 0/T) :-
    activity(A, T, [], _),
    !.
find_FA_FE(A, FA/FE) :-
    activity(A, T, P, _),
    max_FE(P, FA),
    FE is FA + T.

max_FE([A|[]], FE) :-
    find_FA_FE(A, _/FE),
    !.
max_FE([A|T], MFE) :-
    find_FA_FE(A, _/FE1),
    max_FE(T, FE2),
    ((FE1 >= FE2, MFE = FE1);
    (FE2 >= FE1, MFE = FE2)).

predecessors(X, P) :-
    activity(X, _, P, _).

successors(X, S) :-
    findall(A, depends(A, X), S).

depends(A, X) :-
    activity(A, _, P, _),
    member(X, P).
