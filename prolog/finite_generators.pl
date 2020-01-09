% Task 1. Generate the set of all characteristic vectors with given length
% char_vects(N, VS) VS is the set of all Boolean vectors with length N.

% [] is the only Boolean vector with length 0.
char_vects(0, [[]]).

% recursively generate those with length N from those with length N - 1.
% If [H|T] is Boolean vector with length N
% % than T is Boolean vector with length N - 1
% % and either H is 0 or H is 1. 
char_vects(N, VS) :-
    N > 0,
    N1 is N - 1,
    char_vects(N1, TS),
    add_first_to_each(0, TS, ZS),
    add_first_to_each(1, TS, OS),
    append(ZS, OS, VS).

% add_first_to_each(E, L, R) if L is list of lists eg. L = [L1, L2, ..., Lk]
% than R = [ [E|L1], [E|L2], ..., [E|Lk] ].

% if L is empty than R is also empty and we don't care what E is.
add_first_to_each(_, [], []).

% if L is not empty than we add E to the head and recursively add E to the tail of L.
add_first_to_each(E, [L|LS], [ [E|L] | RS ]) :- add_first_to_each(E, LS, RS).

% append(L1, L2, R) R is the concatenation of L1 and L2.
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 2. You are given pair [XC, YC] of integers and R a whole number
% write predicate in_circle([XC, YC], R, [X, Y]) which generates in [X, Y] (one by one)
% integer coordinates of 2D point with inside the Circle with center <XC, YC> and radius R.

% Solution 1. We generate a point with integer coordinates
% inside the square [XC - R, XC + R] x [YC - R, YC + R]
% and check if it is incide the Circle with center <XC, YC> and radius R.

in_circle([XC, YC], R, [X, Y]) :-
    A is XC - R,
    B is XC + R,
    C is YC - R,
    D is YC + R,
    between(A, B, X),
    between(C, D, Y),
    U is X - XC,
    V is Y - YC,
    U * U + V * V =< R * R. % (X - XC)^2 + (Y - YC)^2 <= R^2

% Solution 2. We generate a point with integer coordinates
% inside the square [-R, R] x [-R, R]
% and check if it is incide the Circle with center <0, 0> and radius R
% than we translate the point with (XC, YC).

in_circle([XC, YC], R, [X, Y]) :-
    MR is -R,
    between(MR, R, U),
    between(MR, R, V),
    U * U + V * V =< R * R,
    X is U + XC,
    Y is V + YC.

% between(A, B, X) A and B are integers and X is integer between them,
between(A, B, A) :- A =< B.
between(A, B, X) :- A < B, A1 is A + 1, between(A1, B, X).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 3. You are given two integers A and B and natural N.
% Generate all lists (one by one) with integer elements between A and B and length N.

% Solution. We generate a list with recursion on N.
% list_between(A, B, N, L)
list_between(_, _, 0, []).
list_between(A, B, N, [H|T]) :-
    N > 0,
    N1 is N - 1,
    between(A, B, H),
    list_between(A, B, N1, T).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 4. More general Task 3.
% We want to generate all lists of length N with elements which are elements of given list L.

% Solution we change only how we generate a element. (switch between with member)
% variations_with_repetition(L, N, R) R is list of length N whose elements are all elements of L.
variations_with_repetition(_, 0, []).
variations_with_repetition(L, N, [H|T]) :-
    N > 0,
    N1 is N - 1,
    member(H, L),
    variations_with_repetition(L, N1, T).

% member(X, L) X is member of the list L.
member(H, [H | _]).
member(X, [_ | T]) :- member(X, T).

% Task 3 can be solved with Task 4 if we write range.

% range(A, B, X) A and B are integers X = [A, A + 1, A + 2, ..., B].
range(A, B, []) :- A > B.
range(A, B, [A | T]) :- A =< B, A1 is A + 1, range(A1, B, T).

task3(A, B, N, L) :- range(A, B, R), variations_with_repetition(R, N, L).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 5. Write predicate integer_closed(L, M) such that
% when given list L of integers M is generated using L in such way:
% - SET(M) is subset of SET(L).
% - (forall X element of M) (exists Y element of M) ({X - Y, Y + Y, X * Y} is subset of SET(L)).
% where SET(Y) is the set of elements of the list Y.

% Solution. We A and B are lists we can change SET(A) is subset of SET(B) with
% each member of A is member of B. (We don't care that we do extra work for elements wich repeat)

integer_closed(L, M) :-
    subset(M, L), % with this we covered: SET(M) is subset of SET(L)
    is_integer_closed(M, L).

% is_integer_closed with recurtion
is_integer_closed([], _).
is_integer_closed(M, L) :-
    M = [X|XS],
    is_element_closed(X, M, L), 
    is_integer_closed(XS, L).

% is_integer_closed with recurtion with not
is_integer_closed(M, L) :- not((member(X, L), not(is_element_closed(X, M, L)))).

% is_element_closed(X, M, L) check if there is Y element of M such that {X - Y, Y + Y, X * Y} is subset of SET(L).
is_element_closed(X, M, L) :-
    member(Y, M),
    A is X - Y,
    B is X + Y,
    C is X * Y,
    member(A, L),
    member(B, L),
    member(C, L).
    
% subset(S, M) S is subset of M
% if M is the empty set than there is only one subset of it - the empty set
subset([], []).
subset(S, [_ | T]) :- subset(S, T).
subset([H | S], [H | T]) :- subset(S, T).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 6. If L is list of lists than T is list transversal for L if:
% - each element of T is element of some element of L
% - (forall X element of L) (there are common elements of X and T)

% Solution.

list_trans(L, T) :-
    concat(L, M),
    distinct(D, M),
    subset(T, D),
    is_list_trans(T, L).

% recursive variant
is_list_trans(_, []).
is_list_trans(T, [X|XS]) :- common_elements(X, T), is_list_trans(T, XS).

% variant with not
is_list_trans(T, L) :- not((member(X, L), not(common_elements(X, T)))).

% common_elements(A, B) checks if A and B have common elements.
% one element is enough.
common_elements(A, B) :- member(X, A), member(X, B).

% concat(L, R) If L = [L1, L2, ..., Lk] than R = L1.L2.....Lk.
concat([], []).
concat([L|LS], R) :- concat(LS, T), append(L, T, R).

% distinct(D, L) L is list D is list of the distinct elements of L
distinct([], []).
distinct([H|R], [H|T]) :- distinct(R, T), not(member(H, R)).
distinct(D, [H|T]) :- distinct(D, T), member(H, D).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 7. If L is list of lists than M is minimal list transversal for L if
% - M is trasversal for L
% - there is no trasversal for L wich has smaller length than M

min_list_trans(L, M) :-
    list_trans(L, M),
    list_length(M, K),
    not((list_trans(L, T), list_length(T, S), S < K)).

list_length([], 0).
list_length([_ | T], N) :- list_length(T, M), N is M + 1.

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 8. Given list of lists L generate the length of a minimal transversal.

% Solution 1. Using min_list_trans to generate minimal list transversal for L and than get it's length.

% The ! is for cutting - stop Prolog from backtracking.
% DO NOT USE ON ANY EXAM IF YOU ARE NOT 100% SURE THAT CUTTING
% WILL PREVENT A DIFFERENT SOLUTION TO BE GENERATED !!!
% HERE THE length OF EVERY MINIMAL TRANSVERSAL IS THE SAME !!!
task8_1(L, K) :- min_list_trans(L, M), !, list_length(M, K).

% Solution 2. Use the definition to find the length

task8_2(L, K) :-
    list_trans(L, M),
    list_length(M, K),
    not((list_trans(L, T), list_length(T, S), S < K)),
    !. % if we find one minimal transversal we stop e.g. cut the solution :)

% Solution 3. The worst case for minimal transversal length is to be the length of L
% Example L = [[1], [2], [3]] than M = [1, 2, 3]

task8_3(L, K) :-
    list_length(L, D),
    between(0, D, K),
    list_trans(L, M), % there is list transversal of L
    list_length(M, K), % with length K
    not((list_trans(L, T), list_length(T, S), S < K)),
    !. % stop we have found the length of each minimal transversal :)
  
% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
          
% Task 9. Write predicate partition(L. R) wich when given list L generates in R (one by one)
% each partition of L such that if we concatenate all the elements of R we get L.

% Solution

% The empty list has only one partition wich is empty.
partition([], []).
% To partition L we first split it on two P and S such that L = P.S
% we partition S and use it to construct partition of L.
partition(L, [P|T]) :- append(P, S, L), P \= [], partition(S, T).

% $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

% Task 10. Write predicate that finds the minimum of list of integers.

% Solution 1. Recursive

minimum([X], X).
minimum([H|T], M) :- minimum(T, S), min(H, S, M).

min(A, B, A) :- A =< B.
min(A, B, B) :- B < A.

% Solution 2. not

minimum(L, M) :- member(M, L), not((member(X, L), X < M)).
