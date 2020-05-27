% in_circle_rat([P, Q], [[A, B], [C, D]]) :
% Given [P, Q] wich represents a rational number
% generate point [[A, B], [C, D]] with rational cordinates such that
% it is insice the circle with radius [P, Q] and center (0, 0).

% circle: x^2 + y^2 <= R^2
% (a / b)^2 + (c / d)^2 <= (p / q)^2
% (a^2 / b^2) + (c^2 / d^2) <= p^2 / q^2
% ((a^2.d^2) + (c^2.b^2)) / (b^2 . d^2) <= p^2 / q^2 | . b^2.d^2.q^2
% a^2.d^2.q^2 + c^2.b^2.q^2 <= p^2.b^2.d^2
% (a.d.q)^2 + (c.b.q)^2 <= (p.b.d)^2

gen_in_circle_rat([P, Q], [[A, B], [C, D]]) :-
    gen_pair_rat([A, B], [C, D]),
    (A * D * Q)^2 + (C * B * Q)^2 =< (P * B * D)^2.

gen_pair_rat([P, B], [Q, D]) :-
    gen_4_tuple_nat(A, B, C, D),
    B > 0,
    D > 0,
    gcd(A, B, 1),
    gcd(C, D, 1),
    int(A, P),
    int(C, Q).

gen_KS(1, S, [S]).
gen_KS(K, S, [H | T]) :-
    K > 1,
    between(0, S, H),
    SH is S - H,
    K1 is K - 1,
    gen_KS(K1, SH, T).

gen_4_tuple_nat(A, B, C, D) :-
    nat(S),
    gen_KS(4, S, [A, B, C, D]).

nat(0).
nat(N) :- nat(K), N is K + 1.

int(0, 0).
int(N, N) :- N > 0.
int(N, Z) :- N > 0, Z is -N.

% B = Q * A + R
% A = Q1 * R = R1

gcd(0, B, B) :- B =\= 0.
gcd(A, B, D) :-
    A =\= 0,
    R is B mod A,
    gcd(R, A, D).

% is_graph([V, E]) : checks if [V, E] represents a graph.
% (1, 2) is edge in the graph, [1, 2] is member of E and [2, 1] is not member of E
% V is sorted

% (forall X member of V)(forall Y member of V)
% if [X, Y] is member of E than X < Y and [Y, X] is not member of E

% not not (forall X member of V)(forall Y member of V)
% if [X, Y] is member of E than X < Y and [Y, X] is not member of E

% P => Q is equiv. with: not P or Q

% not (exists X member of V)(exists Y member of V)
% not (not [X, Y] is member of E or (X < Y and [Y, X] is not member of E))

% not (exists X member of V)(exists Y member of V)
% [X, Y] is member of E and  not (X < Y and [Y, X] is not member of E)

is_graph([V , E]) :-
    not((append(_, [A, B | _] , V), A >= B)),
    not((member(X, V), member(Y, V), member([X, Y], E), not((X < Y, not(member([Y, X], E)))))).
