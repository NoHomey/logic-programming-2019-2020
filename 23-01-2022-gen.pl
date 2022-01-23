% gen Generates the longest common subsequence of a list of lists L.
% logest -> there does not exist a common subsequence with larger length.
gen(M, L) :- genCandidate(M, L), len(M, B), not((genCandidate(C, L), len(C, A), A > B)). 
 
% genCandiate generates common subsequence of a list of lists L.
genCandidate(M, L) :- L = [X | _], subset(M, X), commonSubset(M, L).

% subset(X, A) :- X is subset of A
subset([], []).
subset(X, [_ | S]) :- subset(X, S).
subset([E | X], [E | S]) :- subset(X, S).

% commonSubset(X, L) :- X is common subset for each element of L
commonSubset(S, L) :- not((member(X, L), not(subset(S, X)))).

% member generates every element X of a list L.
member(X, [X | _]).
member(X, [Y | T]) :- member(X, T), X \= Y.

% len(X, N) :- N is the length of X.
len([], 0).
len([_ | T], N) :- len(T, N1), N is N1 + 1.
