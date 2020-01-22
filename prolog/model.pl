exists(X , [X | _]).
exists(X, [_ | R]) :- exists(X, R).

f1(All, P) :-
    exists(X, All),
    exists(Y, All),
    exists(Z, All),
    exists([X, Y], P),
    exists([Y, Z], P),
    not(exists([X, Z], P)).

f2(All, P) :-
    exists(X, All),
    exists(Y, All),
    not((exists([X, Y], P), exists([Y, Y], P))).

f31(All, P) :-
    exists(Y, All),
    check_all_rec_f31(Y, All, P, All).

check_all_rec_f31(_, _, _, []).
check_all_rec_f31(Y, All, P, [X | Rest]) :-
    exists([X, Y], P),
    exists(Z, All),
    Z \= Y,
    exists([Y, Z], P),
    check_all_rec_f31(Y, All, P, Rest).

f32(All, P) :- check_all_rec_f32(All, P, All).

check_all_rec_f32(_, _, []).
check_all_rec_f32(All, P, [X | Rest]) :-
    exists(Y, All),
    exists(Z, All),
    exists([Y, Z], P),
    exists([Z, X], P),
    Y \= Z,
    check_all_rec_f32(All, P, Rest).

f41(All, P) :- check_all_rec_f41(All, P, All).

check_all_rec_f41(_, _, []).
check_all_rec_f41(All, P, [X | Rest]) :-
    exists(Y, All),
    exists(Z, All),
    exists([Y, X], P),
    exists([Z, X], P),
    Y \= Z,
    check_all_rec_f41(All, P, Rest).

f42(All, P) :-
    exists(Y, All), 
    check_all_rec_f42(Y, All, P, All).

check_all_rec_f42(_, _, _, []).
check_all_rec_f42(Y, All, P, [X | Rest]) :-
    exists([X, Y], P),
    exists(Z, All),
    Z \= Y,
    exists([Y, Z], P),
    check_all_rec_f42(Y, All, P, Rest).

f5(All, P) :-
    exists(X, All),
    exists(Y, All),
    not((exists([X, Y], P), exists([Y, X], P))).

model11(All, P) :- f1(All, P), f2(All, P), f31(All, P).

model21(All, P) :- model11(All, P), f41(All, P).

model31(All, P) :- model21(All, P), f5(All, P).

model41(All, P) :- model21(All, P), not(f5(All, P)).

model12(All, P) :- f1(All, P), f2(All, P), f32(All, P).

model22(All, P) :- model12(All, P), f42(All, P).

model32(All, P) :- model22(All, P), not(f5(All, P)).

model42(All, P) :- model22(All, P), f5(All, P).

product([], _, []).
product([A | AS], BS, R) :-
    partial_product(A, BS, C),
    product(AS, BS, CS),
    append(C, CS, R).

partial_product(_, [], []).
partial_product(A, [B | BS], [[A, B] | CS]) :-
    partial_product(A, BS, CS).

range(A, B, []) :- A > B.
range(A, B, [A | R]) :- A =< B, A1 is A + 1, range(A1, B, R).

subset([], []).
subset(S, [_ | T]) :- subset(S, T).
subset([H | S], [H | T]) :- subset(S, T).

between(A, B, A) :- A =< B.
between(A, B, C) :- A < B, A1 is A + 1, between(A1, B, C).

genP(N, All, P) :-
    between(0, N, K),
    range(0, K, All),
    product(All, All, Full),
    subset(P, Full).

gen_model11(N, P) :-
    genP(N, All, P), model11(All, P).

gen_model21(N, P) :-
    genP(N, All, P), model21(All, P).

gen_model31(N, P) :-
    genP(N, All, P), model31(All, P).

gen_model41(N, P) :-
    genP(N, All, P), model41(All, P).

gen_model12(N, P) :-
    genP(N, All, P), model12(All, P).

gen_model22(N, P) :-
    genP(N, All, P), model22(All, P).

gen_model32(N, P) :-
    genP(N, All, P), model32(All, P).

gen_model42(N, P) :-
    genP(N, All, P), model42(All, P).
