% append(L1, L2, C) C is L1 . L2
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% reverse(R, L) R is reverse of L.
reverse([], []).
reverse(R, [H|T]) :- reverse(TR, T), append(TR, [H], R).

% remove(X, L, R) R is L from which is removed an occurence of X.
remove(X, L, R) :- append(P, [X|S], L), append(P, S, R).

% insert(X, L, R) R is L in which is inserted X.
insert(X, L, R) :- append(P,S, L), append(P, [X|S], R).

% permutate(P, L) P is permutation of L.
permutate([], []).
permutate(P, [H|T]) :- permutate(Q, T), insert(H, Q, P).

% power_set(P, S) P is the "power set" of L.
power_set([[]], []).
power_set(PS, [X|T]) :- power_set(S, T), add_first(X, S, XS), append(XS, S, PS). 

% add_first(X, LS, RS) LS is list of lists, RS is each list from LS to which X is added as head
% If LS = [L1, L2, ..., Ln], then RS = [[X|L1], [X|L2], ..., [X|Ln]].   
add_first(_, [], []).
add_first(X, [H|T], [[X|H] | R]) :- add_first(X, T, R). 

path(Graph, Start, End, Path) :-
   path_helper(Graph, Start, End, [Start], Path).
path_helper(_, End, End, Visited, Path) :- reverse(Path, Visited).
path_helper(Graph, Current, End, Visited, Path) :-
    Current \= End,
    edge_non_oriented_edges_list(Graph, Current, Next), % edge(Graph, Current, Next)
    non_member(Next, Visited),
    path_helper(Graph, Next, End, [Next|Visited], Path).
   
non_member(_,[]).
non_member(X, [H|T]) :- X \= H, non_member(X, T).

% Graph = [connected(U1, [V1, ..., Vn]), ..., connected(Uk, [T1, ..., Tm])] 
edge_oriented_adjacency_list(Graph, U, V) :-
    member(connected(U, Vs), Graph),
    member(V, Vs).

% Graph = [e(U1, V1), ..., e(Uk, Vk)] 
edge_oriented_edges_list(Graph, U, V) :- member(e(U, V), Graph).

% Graph = [connected(U1, [V1, ..., Vn]), ..., connected(Uk, [T1, ..., Tm])] 
edge_non_oriented_adjacency_list(Graph, U, V) :-
    member(connected(U, Vs), Graph),
    member(V, Vs).
% check for other direction
edge_non_oriented_adjacency_list(Graph, U, V) :-
    member(connected(V, Us), Graph),
    member(U, Us).

% Graph = [e(U1, V1), ..., e(Uk, Vk)] 
edge_non_oriented_edges_list(Graph, U, V) :- member(e(U, V), Graph).
% check for other direction
edge_non_oriented_edges_list(Graph, U, V) :- member(e(V, U), Graph).
