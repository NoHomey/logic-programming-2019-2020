range(A, B, A) :- A =< B.
range(A, B, C) :- A < B, A1 is A + 1, range(A1, B, C).

member(X, [X | _]).
member(X, [Y | T]) :- member(X, T), X \= Y.

p(X) :- range(0, 13, X), X mod 3 =:= 1.

findAllP(Res) :- findAllPStep([], Res), !.    
    
findAllPStep(Curr, Curr) :- not((p(X), not(member(X, Curr)))).
findAllPStep(Curr, Res) :- p(X), not(member(X, Curr)), findAllPStep([X | Curr], Res).
