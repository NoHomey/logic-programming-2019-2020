gen_bin_tree(Tree) :-
    gen_pair(Height, Vertex),
    gen_bin_tree_with_vertex(Height, Vertex, Tree).

gen_pair(N, K) :-
    nat(D),
    between(0, D, N),
    K is D - N.

nat(0).
nat(N) :- nat(K), N is K + 1.

gen_bin_tree_with_vertex(Height, Vertex, Tree) :-
    Limit is Vertex - 1,
    range(0, Limit, Range),
    subset(Range, Set),
    permutate([Vertex | Set], Vertecies),
    gen_bin_tree(Height, Vertecies, Tree, []).

gen_bin_tree(-1, Vertecies, [], Vertecies).
gen_bin_tree(Height, [Vertex | Vertecies], [LeftTree, Vertex, RightTree], RestVerticies) :-
    Height >= 0,
    Limit is Height - 1,
    between(-1, Limit, LeftHeight),
    between(-1, Limit, RightHeight),
    ensure_height(Limit, LeftHeight, RightHeight),
    gen_bin_tree(LeftHeight, Vertecies, LeftTree, AfterLeft),
    gen_bin_tree(RightHeight, AfterLeft, RightTree, RestVerticies).

ensure_height(Height, Height, _).
ensure_height(Height, Left, Height) :- Left =\= Height. 

range(A, B, []) :- A > B.
range(A, B, [A | R]) :-
    A =< B,
    A1 is A + 1,
    range(A1, B, R).

permutate([], []).
permutate([H | T], P) :-
    permutate(T, Q),
    insert(H, Q, P).

insert(X, L, R) :-
    append(P, S, L),
    append(P, [X | S], R).
    
subset([], []).
subset([_ | T], S) :- subset(T, S).
subset([H | T], [H | S]) :- subset(T, S).
