% join(LS, L) : L is join of lists of LS

join([], []).
join([L | LS], R) :- join(LS, T), append(L, T, R).

append([], L, L).
append([H | T], L, [H | R]) :- append(T, L, R).

% partition(P, L) : P is in order partion of L
% P partition L
partition([], []).
partition([ [H]      | P  ], [H | T]) :- partition(P, T).
partition([ [H | HP] | TP ], [H | T]) :- partition([HP | TP], T).
% a, [[ b, c ], [d, e], [f]] -> [ [a, b, c], [d, e], [f] ]

partition_with_append([], []).
partition_with_append([P | PS], L) :-
    append(P, S, L),
    P \= [],
    partition_with_append(PS, S).

% Arithmetics

% = is syntactic equality
% =:= is semantic equality (LH == RH)
% =\= is semantic inequality (LH != RH)
% X is Exp -> Evaluates Exp and assigns the result to X if does not have value.
% X is Exp -> If X has value then Evaluates Exp and checks result with value of X.

% ( C++ X <= Y) -> X =< Y !!! (don't do <=)
% (C++ X >= Y) -> X >= Y  !!! (don't do =>)

% min(M, A, B) -> M is the minimum of A and B
min(A, A, B) :- A =< B.
min(B, A, B) :- A > B.

% min(M, L) -> M is the minimum of elements of list L
min(M, [M]).
min(M, [A, B | T]) :- min(K, [B | T]), min(M, A, K).

% list_length(N, L) -> N is the length of list L
list_length(0, []).
list_length(N, [_ | T]) :- list_length(K, T), N is K + 1.

% list_element_at(X, L, N) -> is X the Nth element of L
list_element_at(X, [X | _], 0).
list_element_at(X, [_ | T], N) :- N > 0, K is N - 1, list_element_at(X, T, K).
% This works becase 3th arg is always valid arithmetic Exp. 
% list_element_at(X, [X | _], N) :- N =:= 0.
% list_element_at(X, [_ | T], N) :- N > 0, list_element_at(X, T, N - 1).

index_in_list(X, [X | _], 0).
index_in_list(X, [_ | T], N) :- index_in_list(X, T, K), N is K + 1.

% is_list_sorted(L) -> checks is L sorted (in some fixed order)

is_list_sorted([]).
is_list_sorted([_]).
is_list_sorted([A, B | T]) :- order(A, B), is_list_sorted([B | T]).

order(A, B) :- A >= B.

bogo_sort(L, S) :- permutate(L, S), is_list_sorted(S).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

quick_sort([], []).
quick_sort([H | T], Sorted) :-
    split(T, H, Less, Bigger),
    quick_sort(Less, Sorted_Less),
    quick_sort(Bigger, Soreted_Bigger),
    append(Sorted_Less, [H | Soreted_Bigger], Sorted).

split([], _, [], []).
split([H | T], X, Less, Bigger) :- split(T, X, L, B), add(H, X, L, B, Less, Bigger).

add(E, X, L, B, [E | L], B) :- order(E, X).
add(E, X, L, B, L, [E | B]) :- not(order(E, X)).

% char_vects(K, VS) -> VS is list of all char (boolean) vects
char_vects(0, [ [] ]).
char_vects(K, VS) :-
    K > 0,
    N is K - 1,
    char_vects(N, TS),
    insert_first(0, TS, Zeros),
    insert_first(1, TS, Ones),
    append(Zeros, Ones, VS).

insert_first(_, [], []).
insert_first(X, [L | LS], [ [X | L] | RS ]) :- insert_first(X, LS, RS). 

% between(X, A, B) -> checks / generates X (integer) between A and B.
% between(A + 0, A, B).
% between(A + 1, A, B).
% ...
% between(B, A, B).

between(A, A, B) :- A =< B.
between(X, A, B) :- A < B, A1 is A + 1, between(X, A1, B).

% range(L, A, B) -> L = [A, A + 1, ..., B]
range([], A, B) :- A > B.
range([A | T] , A, B) :- A =< B, A1 is A + 1, range(T, A1, B).

between_with_range(X, A, B) :- range(R, A, B), member(X, R).

member(X, [X | _]).
member(X, [_ | T]) :- member(X, T).


% list_of_K_elements_between_A_and_B(L, K, A, B) -> L is list with length K and elements each between A and B.
list_of_K_elements_between_A_and_B([], 0, _, _).
list_of_K_elements_between_A_and_B([H | T], K, A, B) :-
    K > 0,
    N is K - 1,
    between(H, A, B),
    list_of_K_elements_between_A_and_B(T, N, A, B). 

% vartions_with_repetitions(V, K, L) -> V is list of length K and each element is element of L
vartions_with_repetitions([], 0, _).
vartions_with_repetitions([H | T], K, L) :-
    K > 0,
    N is K - 1,
    member(H, L),
    vartions_with_repetitions(T, N, L).

list_of_K_elements_between_A_and_B_with_range(L, K, A, B) :- range(R, A, B), vartions_with_repetitions(L, K, R).


% gen_K_sum_S(K, S, L) -> L is list with length K and elements naturals with sum S.
gen_K_sum_S(1, S, [S]) :- S >= 0.
gen_K_sum_S(K, S, [H | T]) :- K > 1, N is K - 1, between(H, 0, S), M is S - H, gen_K_sum_S(N, M, T).