% Write predicate miss(X) that succedes only if X is not an element of the sequence:
% A0 = 0
% A1 = 1
% AN+2 = 3 (AN+1)^3 + 2 (AN)^2.

% elem(N, A)
elem(0, 0).
elem(1, 1).
elem(N, A) :- N >= 2, N1 is N-1, N2 is N-2, elem(N1, A1), elem(N2, A2),
    A is 3 * A1 * A1 * A1 + 2 * A2 * A2.

miss(X) :- range(0, X, N), not(elem(N, X)).
