% is_list_transversal(L, T)
% where L is list of lists
% T is list
% and T is list transversal for L

% M is a set of sets
% R is transversal for M if
% (forall S in M)(S intersect R is not empty set)

% (forall X member of L)(exists A : A is member in X and A is member of T)

% =, \=, is, =:=, =\=, <, >, =<, >=, div, mod
% X is 7 div 3 -> X = 2, [], [H | T], terms ... edge(U, V, Weight), ...

is_list_transversal([], _).
is_list_transversal([X | XS], Transversal) :-
    member(A, X),
    member(A, Transversal),
    !,
    is_list_transversal(XS, Transversal).

% (forall X member of L)(exists A member of T and A is member of X)
% not not (forall X member of L)(exists A member of T and A is member of X)
% not (exists X member of L and not (exists A member of T and A is member of X))

% (exists x in S) phi(x) is equiv. with exists x (x in S and phi(x))
% (forall x in S) phi(x) is equiv. with forall x (if x in S then phi(x)) 

% not P => Q ~ P and not Q

is_list_transversal_with_not(L, T) :-
    not((member(X, L), not((member(A, T), member(A, X))))).

% gen_list_transversal(L, T) ->
% we genarate T such that it is list transversal for L, (L is list of lists)

% L = [[a, b ],[a, c], [b, c, d]], -> length(T) = 3
% gen_list_transversal([], []).
% gen_list_transversal([X|XS], [H|R]) :-
%   member(H, X),
%   gen_list_transversal(XS, R).
%  here we have problem: each transversal has fixed length!

% (forall E in T)(E is member of some element of L)
% (forall E in T)(E is member of (union of the elements of L))
% T is subsequnece of (union of the elements of L)

gen_list_transversal(L, T) :-
    union_of_elements(U, L), % U is the union of elements of L
    subsequence(T, U), % T is subsequnce of U
    is_list_transversal(L, T).

% join([], []).
% join(U, [L | LS]) :- join(R, LS), append(L, R, U).

union_of_elements([], []).
union_of_elements(U, [L | LS]) :-
    union_of_elements(R, LS),
    set_minus(L, R, Temp),
    append(Temp, R, U).

% set_minus(S, M, T) -> T = S \ M.
% set_minus(S, [], S).
% set_minus(S, [X | R], U) :-
%    set_minus(S, R, Y),
%    remove(X, Y, U).

set_minus([], _, []).
set_minus([X | S], M, R) :-
    set_minus(S, M, R),
    member(X, M).
set_minus([X | S], M, [X | R]) :-
    set_minus(S, M, R),
    not(member(X, M)).

% set_minus([], _, []).
% set_minus([X | S], M, U) :-
%     set_minus(S, M, R),
%     conditional_add(X, R, M, U).

% % conditional_add(X, A, B, R) -> R is (A union B \ [X])
% conditional_add(X, A, B, A) :- member(X, B).
% conditional_add(X, A, B, [X | A]) :- not(member(X, B)).

subsequence([], []).
subsequence(S, [_ | T]) :-
    subsequence(S, T).
subsequence([H | S], [H | T]) :-
    subsequence(S, T).

% M is minimal transversal for set of set S if
% 1. M is transversal of S
% 2. (forall X in P(M))(X is transversal => X = M)

% M is minimal transversal for set of set S if
% 1. M is transversal of S
% 2. (forall x in M)(M \ {x} is not transversal for S)

% not not (forall x in M)(M \ {x} is not transversal for S)
% not (exists x in M and not (M \ {x} is not transversal for S))
% not (exists x in M and (M \ {x} is transversal for S))

gen_list_min_transversal(L, M) :-
    gen_list_transversal(L, M),
    not((
        member(X, M),
        append(P, [X | R], M),
        append(P, R, U),
        is_list_transversal(L, U)
    )).

% list_length(List, Length)
list_length([], 0).
list_length([_ | T], N) :- list_length(T, M), N is M + 1.

gen_length_of_min_list_transversal(L, K) :-
    gen_list_min_transversal(L, M),
    list_length(M, K).

% if M is min transversal for S
% then if K is length of M then there does not exist transversal T of S with length K - 1.
% the above is not true!

gen_minimal_length_of_transversal(L, K) :-
    list_length(L, N),
    between(1, N, K),
    gen_list_min_transversal(L, M),
    list_length(M, K),
    K1 is K - 1,
    not((
        gen_list_min_transversal(L, T),
        list_length(T, K1)    
    )).
    
% gen_K_subsequnce(L, K, S)
% S is a subsequnce of L with length K

% gen_K_subsequnce(_, 0, []).
% gen_K_subsequnce(L, K, [H | T]) :-
%     K > 0,
%     member(H, L),
%     K1 is K - 1,
%     gen_K_subsequnce(L, K1, T).

gen_K_subsequnce(_, 0, []).
gen_K_subsequnce([_ | T], K, S) :-
    K > 0,
    gen_K_subsequnce(T, K, S).
gen_K_subsequnce([H | T], K, [H | S]) :-
    K > 0,
    K1 is K - 1,
    gen_K_subsequnce(T, K1, S).