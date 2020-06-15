% [] -> empty tree or tree with height -1
% [Left, Root, Right] -> bin tree with root Root, left subtree Left and right subtree Right
% [Left, Root, Right] -> height is the max(height of Left, height of Right) + 1
% [[], Value, []] -> Leaf (tree with height 0)

% parametrization: height, max vertex, count of vertecies

gen_bin_tree(Tree) :-
    gen_nat_triple(Height, Max, K),
    gen_bin_tree(Height, Max, K, Tree).

gen_nat_triple(A, B, C) :-
    gen_nat(N),
    gen_KS(3, N, [A, B, C]).

% gen_KS(K, S, L) -> L is list of K naturals with sum S
gen_KS(1, S, [S]).
gen_KS(K, S, [H | T]) :-
    K > 1,
    K1 is K - 1,
    between(0, S, H),
    SH is S - H,
    gen_KS(K1, SH, T).

gen_nat(0).
gen_nat(N) :- gen_nat(K), N is K + 1. 

% gen_bin_tree(Height, Max, K, Tree) ->
% Tree is bin tree with K vertecies, height is Height,
% vertex with max value for vertex Max or
% Max is a vertex of Tree


% if whe choose K from 0 to Max (we repeat set (list) of vertecies)
% [2, 3, 8] -> choose 3 from 0 to 10 (Max = 10) (<3, 10> =\= <3, 120>)
% [2, 3, 8] -> choose 3 from 0 to 120 (Max = 120)
% [2, 3, 8] -> choose 3 from 0 to 2135 (Max = 2135)

% if we force Max to be max element then we do not repeat
% K, M and C, N and <K, M> =\= <C, N>
% and L is list gen based on K, M
% and L is list gen based on C, N

% case 1. K =\= C then length of L is K = C (contradiction)
% case 2. K =:= C and M =\= N. WLG (M < N) -> max element of L is M and max element of L is N then M = N (contradiction)

gen_bin_tree(Height, Max, K, Tree) :-
    Max1 is Max - 1,
    K1 is K - 1,
    choose_KN(K1, Max1, S),
    permutate([Max | S], V),
    gen_bin_tree_with_vertecies(Height, V, Tree).

gen_bin_tree_with_vertecies(Height, V, Tree) :-
    gen_bin_tree_with_vertecies_helper(Height, V, Tree, []).

gen_bin_tree_with_vertecies_helper(-1, V, [], V).
gen_bin_tree_with_vertecies_helper(H, [U | US], Tree, Rest) :-
    H >= 0,
    H1 is H - 1,
    between(-1, H1, HeightOfLeft),
    between(-1, H1, HeightOfRight),
    check_heights(HeightOfLeft, HeightOfRight, H1),
    gen_bin_tree_with_vertecies_helper(HeightOfLeft, US, LeftTree, ForRight),
    gen_bin_tree_with_vertecies_helper(HeightOfRight, ForRight, RightTree, Rest),
    Tree = [LeftTree, U, RightTree].

check_heights(H, _, H).
check_heights(L, H, H) :- L < H.

choose_KN(K, N, S) :- choose_from(K, 0, N, S).

choose_from(0, _, _, []).
choose_from(K, B, E, [H | T]) :-
    K > 0,
    K1 is K - 1,
    between(B, E, H),
    H1 is H + 1,
    choose_from(K1, H1, E, T).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% For HomeWork
% Task 1. gen_K_Tree(K, Tree) -> By given K in Tree is generated
%                                every K-ary Tree with vertecies natural numbers
% Task 2. gen_Tree(Tree) -> In Tree is generated
%                           every (Rooted) Tree with vertecies natural numbers

% Now
% gen_prime(P)

gen_prime(P) :-
    gen_nat(P),
    is_prime(P).

is_prime(P) :-
    P > 1,
    P1 is P - 1,
    not((
        between(2, P1, D),    
        P mod D =:= 0
    )).

% Q(i) -> count of primes of kind 6k + 1 wich are less then i
% check(X) <-> X = i + Q(i) for some i (positive natural)

q(I, K) :- primes(I, PS), length(PS, K).

check(X) :- between(1, X, I), q(I, K), X =:= I + K.

primes(I, PS) :-
    I1 is I - 1,
    range(2, I1, L),
    filter(L, PS).

filter([], []).
filter([N | NS], T) :- not(condition(N)), filter(NS, T).
filter([N | NS], [N | T]) :- condition(N), filter(NS, T).

condition(X) :- (X mod 6 =:= 1), is_prime(X).

range(A, B, []) :- A > B.
range(A, B, [A | T]) :- A =< B, A1 is A + 1, range(A1, B, T).

% For homework:

% list X is special if every element of X is 3 element list
% list Y is X-coherent if X is special list and
%                         for each A and B elements of Y [A, A, B] is element of X

% Example
% Y = [1, 2]
% X = [[1, 1, 1], [1, 1, 2], [2, 2, 1], [2, 2, 2], [3, 3, 3]]
% Y is X-coherent

% Implement check(X, Y) wich checks is Y is X-coherent list
% Implement split_check(X, Y) wich checks if Y = P o S and P and S are X-coherent
% exists P and S such that Y = P o S and P is X-coherent and S is X-coherent.
% where L o K is the concatenation of L and K