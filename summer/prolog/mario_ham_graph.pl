append([], A, A).
append([H | T], L, [H | A]) :- append(T, L, A).

member(A, [A|_]).
member(A, [_|L]) :- member(A, L).

repeated(A, [A|L]) :- member(A, L).
repeated(A, [_|L]) :- repeated(A, L).

is_graph([V,E]) :-
    not((append(_, [A, B | _], V), A >= B)),
    not((member([A, B], E), not((member(A, V))))),
    not((member([A, B], E), member([B, A], E))),
    not((member([A,A], E))),
    not((repeated(_, E))).

remove(E, A, B) :-
    append(X, [E | Y], A),
    append(X, Y, B).

is_hamiltonian([V, E]) :-
    is_graph([V, E]),
    member(X, V),
    member([X,Y], E),
    remove(X, V, L),
    hamCycle(X, Y, [V,E], L).

hamCycle(X, Y, [V, E], L) :-
    member([Y, Z], E),
    remove(Y, L, L1),
    hamCycle(X, Z, [V, E], L1).

hamCycle(X, Y, [_,E], [Y]) :-
    member([Y,X], E).