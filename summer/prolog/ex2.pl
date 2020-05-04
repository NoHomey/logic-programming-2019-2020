list(X) :- X = [].
list(X) :- X = [_ | Tail], list(Tail).

% member(X, L) : X is member of list L

member(H, [H | _]). % H is member if it the first element.
member(X, [_ | T]) :- member(X, T). % X is member if it element of the tail of the list (rest)

% append(L1, L2, L3) : L3 = L1 . L2
% append([a, b], [c, d], L) : L = [a, b, c, d]
% L = [a, b, c, d] -> L = [a | [b, c, d]]
% -> L =  [a | [b | [c, d]] ]
% append([], [c, d], L) : L = [c, d]
% append([b], [c, d], L) : L = [b | [c, d]] = [b, c, d]
% append([a], [b, c, d], L) : L = [a | [b, c, d]] = [a, b, c, d]

append([], L, L).
append([H | T], L, [H | R]) :- append(T, L, R).

% replace(X, Y, L, R) : R is L where X is replaced with Y
replace(_, _, [], []).
replace(X, Y, [X | T], [Y | R]) :- replace(X, Y, T, R).
replace(X, Y, [H | T], [H | R]) :- replace(X, Y, T, R), X \= H.

% first(X, L) : X is first of the list L
first(X, [X | _]).

% last(X, L) : X is the last element in L
last_rec(X, [X]).
last_rec(X, [_ | T]) :- last_rec(X, T).

% last_rec(X, [c | []]) :- last_rec(X, []) -> false

last(X, L) :- append(_, [X], L).

% prefix(P, L) : P is prefix L
% (EAI) Prefix(L) = {x in S^* | (exist y in S^*)(x.y in L) }

prefix(P, L) :- append(P, _, L).

sufix(S, L) :- append(_, S, L).

% sublist(S, L) S is sublist of L
% sublist -> poredica
% (EAI) Subword(L) = { y in S^* | (exists x, z in S^*)(x.y.z in L)  }
% S is sublist of L if S is sufix of prefix of L
sublist(S, L) :- prefix(P, L), sufix(S, P).
% sublist(S, L) :- sufix(P, L), prefix(S, P). (OK)
% sublist(S, L) :- sufix(S, P), prefix(P, L). (NOT OK)

% sublist_loop(S, L) :- append(_, S, X), append(X, _, L). inifinite
% sublist_loop(S, L) :- append(X, _, L), append(_, S, X). finite

% reverse(R, L) : R is the reverse list of L
reverse([], []).
reverse(Result , [Head | Tail]) :-
    reverse(Temp, Tail),
    append(Temp, [Head], Result).

reverse_accum(Accum, [], Accum).
reverse_accum(Result, [H | T], Accum) :-
    reverse_accum(Result, T, [H | Accum]).

reverse_accum(R, L) :- reverse_accum(R, L, []).

% palindrome(L) : is true IFF L is palindrome
palindrome(L) :- reverse(L, L).

% remove_first(X, L, R) : R is L from which the first occurence of X is removed
remove_first(X, [X | T], T).
remove_first(X, [H | T], [H | R]) :- X \= H, remove_first(X, T, R).
% R = [H | T] ~ append([H], T, R)

remove(_, [], []).
remove(X, [X | T], T).
remove(X, [H | T], [H | R]) :- X \= H, remove(X, T, R).

remove_all(_, [], []).
remove_all(X, [X | T], R) :- remove_all(X, T, R).
remove_all(X, [H | T], [H | R]) :- X \= H, remove_all(X, T, R).

% insert(X, L, R) R is L in which in some position is inserted X 
insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% permutate(P, L) : P is permutation of L ( P perm L )
permutate([], []). % <>, <a, b>
permutate(P, [H | T]) :- permutate(Q, T), insert(H, Q, P).

% f(0)  = 1
% f(n + 1) = (n + 1) . f(n)

% subsequence(S, L) -> S is subsequence of L
% subsequence([b, d], [a, b, c, d, e])
% subsequence([a, c, e], [a, b, c, d, e])

subsequence([], []).
subsequence(S, [_ | T]) :- subsequence(S, T).
subsequence([H | S], [H | T]) :- subsequence(S, T).

% power_set(P, S) P is the "power set" of S (S has not repetitions)

power_set([[]], []). % P({}) = { {} }
% P({a} U S) = P(S) U (Union (M in P(S)) ({a} U M))
power_set(P, [A | S]) :-
    power_set(B, S), % P(S)
    prepend_to_all(A, B, C), % Union (M in P(S)) ({a} U M)
    append(B, C, P). % U (union)

prepend_to_all(_, [], []).
prepend_to_all(X, [L | LS], [[X | L] | RS]) :-
    prepend_to_all(X, LS, RS).