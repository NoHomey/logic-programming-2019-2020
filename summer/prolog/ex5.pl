% gen_KS(K, S, L) -> L is list of K nats with sum S.
gen_KS(1, S, [S]).
gen_KS(K, S, [H | T]) :-
    K > 0,
    between(0, S, H),
    N is S - H,
    K1 is K - 1,
    gen_KS(K1, N, T).
    
gen_fin_seq_Nat(L) :-
    gen_pair(K, S),
    gen_KS(K, S, L).

gen_pair(A, B) :-
    nat(N),
    gen_KS(2, N, [A, B]).

nat(0).
nat(X) :- nat(N), X is N + 1.

% gen_fin_subset_Nat(S)
gen_fin_subset_Nat([]).
gen_fin_subset_Nat(S) :-
    gen_fin_seq_Nat(S),
    encodes_subset(S).

encodes_subset([]).
encodes_subset([_]).
encodes_subset([A, B | T]) :-
    A < B,
    encodes_subset([B | T]).

encodes_subset_not(L) :-
    not((append(_, [A, B | _], L), A >= B)).

% gen_ar_prog(P) -> P is arithmetic progression
gen_ar_prog([]).
gen_ar_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L > 0,
    int(K, A0),
    int(M, D),
    gen_ar_prog(L, A0, D, P).

gen_ar_prog(1, A0, 0, [A0]).
gen_ar_prog(L, A0, D, P) :-
    L > 1,
    gen_ar_prog_with_rec(L, A0, D, P).

gen_ar_prog_with_rec(1, A0, _, [A0]).
gen_ar_prog_with_rec(L, A0, D, [A0 | P]) :-
    L > 1,
    L1 is L - 1,
    A1 is A0 + D,
    gen_ar_prog_with_rec(L1, A1, D, P).

%int(N, Z) -> if N = 0 then Z = 0 else Z is in {N, -N}

int(0, 0).
int(N, N) :- N > 0.
int(N, Z) :- N > 0, Z is -N.

% q(A, [X, Y], [Z, U]) -> given A - natural number
% generate [X, Y], [Z, U] wich represent rational numbers
% such that Y > X > 0, Z > U > 0
% and (X / Y) * (Z / U) = 2
% X + Z < A


% cach1: the cond (X / Y) * (Z / U) = 2 must be checked in integers
% X * Z = 2 * Y * U
% cach2: when [A, B] represents rational number ?
% A is integer, B in none zero natural number and GCD(A, B) = 1

q(A, [X, Y], [Z, U]) :-
    gen_two_positive_rat_numbers([X, Y], [Z, U]),
    Y > X, Z > U,
    X + Z < A,
    X * Z =:= 2 * Y * U.

% B = Q * A + R
% A = Q1 * R + R1
% ...

gcd(0, B, B).
gcd(A, B, D) :-
    A > 0,
    R is B mod A,
    gcd(R, A, D).

gen_4_tuples_nats(A, B, C, D) :-
    nat(N),
    gen_KS(4, N, [A, B, C, D]).

gen_two_positive_rat_numbers([X, Y], [Z, U]) :-
    gen_4_tuples_nats(X, Y, Z, U),
    X > 0,
    Y > 0,
    Z > 0,
    U > 0,
    gcd(X, Y, 1),
    gcd(Z, U, 1).

% gen_in_circle([XC, YC], R, [X, Y])
% given integers XC, YC and R, R is positive
% generate integers X and Y such that
% (X, Y) is point inside the circle with center (XC, YC) and radius R.

% solution 1: generate (A, B) inside the circle with center (0, 0) and radius R.
% (X, Y) = (A + XC, B + YC)
% (A, B) : -R <= A <= R, -R <= B <= R, A^2 + B^2 <= R^2

gen_in_square(R, A, B) :-
    MR is -R,
    between(MR, R, A),
    between(MR, R, B).

gen_in_circle_00(R, A, B) :-
    gen_in_square(R, A, B),
    A^2 + B^2 =< R^2.

gen_in_circle1([XC, YC], R, [X, Y]) :-
    gen_in_circle_00(R, A, B),
    X is A + XC,
    Y is B + YC.

% solution 2: (X - XC)^2 + (Y - YC)^2 <= R^2
% square: [XC - R, XC + R] x [YC - R, YC + R]

% gen_int_closed(L, M): given list of integers L generate M such that
% SET(M) is subset SET(L)
% M represents set (there is no element repetition)
% M is integer closed: (forall a in M)(exist b in L)({a + b, a - b, a * b} is subset of M)

% example L = [1, 2, 0], M = [0], M = [1, 2]

gen_int_closed(L, M) :-
    distinct_elements(L, S),
    subset(S, M),
    is_integer_closed_with_rec(M, S).

subset([], []).
subset([_ | T], S) :- subset(T, S).
subset([H | T], [H | S]) :- subset(T, S).

distinct_elements([], []).

distinct_elements([H | T], R) :-
    distinct_elements(T, D),
    insert_distinct(H, D, R).

insert_distinct(E, D, D) :- member(E, D).
insert_distinct(E, D, [E | D]) :- not(member(E, D)).

is_integer_closed_with_rec(M, L) :- is_integer_closed_with_rec_helper(M, M, L).

is_integer_closed_with_rec_helper([], _, _).
is_integer_closed_with_rec_helper([H | T], M, L) :-
    is_integer_closed_with_rec_helper(T, M, L),
    is_element_closed(H, M, L).

is_element_closed(E, M, L) :-
    member(X, M),
    A is E + X,
    B is E - X,
    C is E * X,
    member(A, L),
    member(B, L),
    member(C, L).

% not not (forall a in M)(exist b in L)({a + b, a - b, a * b} is subset of M)
% not [(exists a in M) not (exist b in L)({a + b, a - b, a * b} is subset of M)]
% not [(exists a in M) not is_element_closed(a, M, L)]
is_integer_closed_with_not(M, L) :-
    not((member(A, M), not(is_element_closed(A, M, L)))).








