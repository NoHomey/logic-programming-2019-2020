% Task 1. q(A, [M, N], [K, L]) We are given whole number A
% and we want to generate all representations of rational numbers
% [M, N] and [K, L] wich represent the rational numbers M / N and K / L.
% Such that:
% - N > M > 0 and K > L > 0 ;
% - (M / N) * (K / L) = 2;
% - M + K < A

% Solution.

% First if [A, B] is representations of rational number
% than gcd(A, b) = 1 in mathematical sense.

% Second (M / N) * (K / L) = 2 is condition wich we NEED to check in INTEGERS
% We check the EQUIVALENT: MK = 2NL.

% Third we can not use the build in function gcd(X, Y).
% We need to write our own. We do that by writng predicate !!!

gcd(A, 0, A) :- A =\= 0.
gcd(0, B, B) :- B =\= 0.
gcd(A, B, D) :- A =\= 0, B =\= 0, R is mod(A, B), gcd(B, R, D).

q(A, [M, N], [K, L]) :-
    nat(D),
    gen_KS(4, D, [M, N, K, L]),
    M > 0, N > M,
    L > 0, K > L,
    gcd(M, N, 1),
    gcd(K, L, 1),
    M * K =:= 2 * N * L,
    M + K < A.

nat(0).
nat(X) :- nat(Y), X is Y + 1.
 
between(A, A, B) :- A =< B.
between(X, A, B) :- A < B, A1 is A + 1, between(X, A1, B).
 
gen_KS(1, S, [S]).
gen_KS(K, S, [H|T]) :-
    K > 1,
    K1 is K - 1,
    between(H, 0, S),
    SH is S - H,
    gen_KS(K1, SH, T).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 2. Generate all finite arithmetic progresions of integers

% Solution 1 finite arithmetic progresion is determinated by it's:
% - length
% - starting value
% - step.

% So we will generate all triples of naturals [K, N, M]
% than get the Nth integer A and the Mth integer S
% and generate the finite arithmetic progresion determinate by K, A and S.

% BUT we wish to not repeat progressions
% Every time the length is 0 no matter A and S we will generate []
% So we add it as first rule for generating and than ensure K is > 0.

% But If K is 1 no matter S we will generate [A]
% and we wish to generate it only once
% So when we generate progression we will generate [A] only if S is 0 !!!

gen_ar_prog([]).
gen_ar_prog(P) :-
    nat(D),
    gen_KS(3, D, [K, N, M]),
    K > 0,
    int(A, N),
    int(S, M),
    ar_prog(A, S, K, P).
 
ar_prog(A, 0, 1, [A]).
ar_prog(A, S, K, P) :- K > 1, gen_prog(A, S, K, P).

gen_prog(A, _, 1, [A]).
gen_prog(A, S, K, [A|P]) :-
    K > 1,
    K1 is K - 1,
    AS is A + S,
    gen_prog(AS, S, K1, P).

int(Z, N) :- mod(N, 2) =:= 0, Z is div(N, 2).
int(Z, N) :- mod(N, 2) =:= 1, Z is - div(N, 2) - 1.

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 3. Generate all finite subsets of the Naturals.

% Solution. We generate all lists of naturals and filter them.
% We select only those lists wich encode a valid subset.
% We do not want to repeat elements so without loss of generality
% we can think that our set's occur to us SORTED Asc.
% This is so once we generate [1, 2] we do not generate [2, 1].
% We can also think that from each are removed the duplicates.
% This is because we don't want to generate [1, 1, 2] but we want [1, 2].

% So combining the above we get that we want to generate all list of naturals
% wich are strongly increasing !

fin_subset_Nat(L) :- list_Nat(L), encodes_set(L).

list_Nat([]).
list_Nat(L) :- nat(N), gen_KS(2, N, [K, S]), K > 0, gen_KS(K, S, L).

% recursive
encodes_set([]).
encodes_set([_]).
encodes_set([A, B | RS]) :- A < B, encodes_set([B | RS]).

% not
encodes_set(L) :- not((append(_, [A, B | _], L), A >= B)).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
