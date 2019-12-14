% min(X, A, B) - X is min(A, B)
min(A, A, B) :- A =< B.
min(B, A, B) :- A > B.

% mod2(R, N) - R is N mod 2
mod2(0, 0).
mod2(1, 1).
mod2(R, N) :- N > 1, N2 is N - 2, mod2(R, N2).

% mod(R, N, K) - R is N mod K
mod(N, N, K) :- N < K.
mod(R, N, K) :- N >= K, NK is N - K, mod(R, NK, K).

list_length(0, []).
list_length(N, [_|T]) :- list_length(T, M), N is M + 1.

elment_at(E, 0, [E | _ ]).
element_at(E, N, [_ | T]) :- N > 0, N1 is N - 1, element_at(E, N1, T). 

is_sorted([]).
is_sorted([_]).
is_sorted([E1, E2|T]) :- is_sorted([E2|T]), E1 =< E2.

is_sorted_with_not(L) :- not(append(_, [A, B | _], L), A > B).

% insert(X, L, R) R is L in which is inserted X.
insert(X, L, R) :- append(P,S, L), append(P, [X|S], R).

% permutate(L, P) P is permutation of L.
permutate([], []).
permutate([H|T], P) :- permutate(T, Q), insert(H, Q, P).

% bogo_sort(L, S) - S is L sorted according to is_sorted
bogo_sort(L, S) :- permutate(L, S), is_sorted(S). 

quick_sort([], []).
quick_sort([H|T], S) :-
    partition(T, H, L, B),
    quick_sort(L, SL),
    quick_sort(B, SB),
    append(SL, [H|SB], S).

partition([], _, [], []).
partition([H|T], E, [H|L], B) :- H =< E, partition(T, E, L, B).
partition([H|T], E, L, [H|B]) :- H > E, partition(T, E, L, B).

% range(R, A, B) - R is [A, A + 1, ..., B]
range([], A, B) :- A > B.
range([A|T], A, B) :- A =< B, A1 is A + 1, range(T, A1, B).

% between(X, A, B) - X is a integer between A and B
between(A, A, B) :- A =< B.
between(X, A, B) :- A < B, A1 is A + 1, between(X, A1, B).

between_with_range(X, A, B) :- range(R, A, B), member(X, R).

% definition for natural we use to generate all nats: 0, 1, 2, 3, ...
nat(0).
nat(N) :- nat(M), N is M + 1.

% standart counting of the integers: n -> n / 2 if n is even and n -> (1 - n)/2 - 1, if n is odd
int(Z, N) :- mod2(0, N), Z is N // 2.
int(Z, N) :- mod2(1, N), M is (N - 1) // 2, Z is -M - 1.

% generate all integers according to their standart counting: 0, -1, 1, -2, 2, -3, 3, ...
gen_int(Z) :- nat(N), int(Z, N).         
         
% generate all pairs of nats using Cantor counting
pair_nats(A, B) :- nat(D), between(A, 0, D), B is D - A.

% generate all pairs of ints we reuse the idea for gen_int only this time we generate pair of nats which we map to ints.
pair_ints(A, B) :- pair_nats(M, K), int(A, M), int(B, K).

% gen_KS(L, K, S) - L is list of K nats with sum S.
gen_KS([S], 1, S).
gen_KS([H|T], K, S) :- K > 1, between(H, 0, S), K1 is K - 1, SH is S - H, gen_KS(T, K1, SH).

% gen_K_nats(T, K) T is K-tuple of nats, we use the same idea as pairs_nats we generate all sums D and then all elements with sum of coordinates D.
gen_K_nats(T, K) :- nat(D), gen_KS(T, K, D).

% we generate all elements of Union |N^k for k in |N^+ (positive natural)
gen_uion_Nat_K(T) :- pair_nats(K, S), K > 0, gen_KS(T, K, S).
