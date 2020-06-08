% admisible_graph_color([V, E], K, C)
% K is the number of the colors
% C is K a coloring if list of pairs
% ([U, M] - U is vertex, M is natural number less than K)

% C is K admisible coloring of the vertecies if
% (forall U in V)(forall W in V)( if <U, W> is in E  then C[U] =\= C[W])
% and C is a fuction from the set of vertecies (V) to {0, 1, ..., K - 1} representation
% (forall U in V)(forall M, S in {0, 1, ..., K - 1})
%       ( if [U, M], [U, S] are members of C then M = S )

graph_color_helper([], _, []).

graph_color_helper([V | VS], K, [[V, M] | T]) :-
    graph_color_helper(VS, K, T),
    S is K - 1,
    between(0, S, M).

% not not (forall U in V)(forall W in V)( if <U, W> is in E  then C[U] =\= C[W])
% not (exists U in V)(exists W in V) not ( if <U, W> is in E  then C[U] =\= C[W])
% not (exists U in V)(exists W in V) not ( not (<U, W> is in E) or C[U] =\= C[W])
% not (exists U in V)(exists W in V) ( <U, W> is in E and C[U] =:= C[W] )

gen_K_admisible_coloring([V, E], K, C) :-
    graph_color_helper(V, K, C),
    not((
        member([U, W], E),
        member([U, M], C),
        member([W, S], C),
        M =:= S
    )),
    T is K - 1,
    not((
        between(0, T, M),
        not(member([_, M], C))
    )).

% chrom_number(G, K) -> K is the chromatic number of G

% This uses knowldge about in which order between(1, N, K) generates
% chrom_number([V, E], K) :-
%     list_length(V, N),
%     between(1, N, K),
%     gen_K_admisible_coloring([V, E], K, _),
%     !.

chrom_number([V, E], K) :-
    list_length(V, N),
    between(1, N, K),
    gen_K_admisible_coloring([V, E], K, _),
    K1 is K - 1,
    not(gen_K_admisible_coloring([V, E], K1, _)),
    !.

list_length([], 0).
list_length([_ | T], N) :- list_length(T, M), N is M + 1.

% path([V, E], S, F, P) -> P is path in the graph [V, E] between S and F

path([V, E], S, F, P) :-
    subseq(M, V),
    member(S, M),
    member(F, M),
    permutate(P, M),
    is_path([V, E], P).

edge([U, W], E) :- member([U, W], E).
edge([U, W], E) :- member([W, U], E).

subseq([], []).
subseq(S , [_ | T]) :- subseq(S, T).
subseq([H | S] , [H | T]) :- subseq(S, T).

permutate([], []).
permutate(P , [H | T]) :- permutate(Q, T), insert(H, Q, P).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

is_path([_, _], [_]).
is_path([V, E], [U, W | P]) :-
    is_path([V, E], [W | P]),
    edge([U, W], E).

%  S = 2, F = 3 -> [2, 1, 3]

% Step 1: [2]
% edge([2, 1], E),
% Visited = [1 | [2]] = [1, 2]

% Step 2: [1, 2]
% edge([1, 3], E),
% Visited = [3 | [1, 2]] = [3, 1, 2] -> [2, 1, 3]

path_imp([_, E], S, F, P) :- path_imp_track_viseted(E, F, [S], P).

path_imp_track_viseted(_, F, [F | Visited], P) :- reverse([F | Visited], P).
path_imp_track_viseted(E, F, [T | Rest], P) :-
    T \= F,
    edge([T, U], E),
    not(member(U, Rest)),
    path_imp_track_viseted(E, F, [U, T | Rest], P).

path([V, E], P) :-
    member(S, V),
    member(F, V),
    S \= F,
    path_imp([V, E], S, F, P). 

is_connected([V, E]) :-
    not((
        member(S, V),
        member(F, V),
        S \= F,
        not(path_imp([V, E], S, F, _))
    )).

is_acyclic([V, E]) :-
    not((
        member(S, V),
        member(F, V),
        S \= F,
        path_imp([V, E], S, F, P1),
        path_imp([V, E], S, F, P2),
        P1 \= P2
    )).

is_tree(G) :-
    is_connected(G),
    is_acyclic(G).

is_bin_tree([V, E]) :-
    is_tree([V, E]),
    member(R, V),
    is_bin_tree_with_root([V, E], R),
    !.

% is_bin_tree_with_root([V, E], R) :- neighbors(R, [C, E], Vertices).

% For Home work