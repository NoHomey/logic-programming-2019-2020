% nat(X). zero, s(zero), s(s(zero)), s(s(s(zero))), ...
% 0 in N, n in N -> n + 1 in N
% nat(X) :- X = zero.
nat(zero).
% nat(Z) :- Z = s(X), nat(X).
nat(s(X)) :- nat(X).

% nat(s(s(s(zero)))), s(X) = s(s(s(zero))), X = s(s(zero)), nat(s(s(zero))).
% nat(s(s(zero))), s(X) = s(s(zero)), X = s(zero), nat(s(zero)).
% nat(s(zero)), s(X) = s(zero), X = zero, nat(zero).
% nat(zero) -> true.

% add(X, Y, Z) Z is X "+" Y
% 0 + Y = Y
% (1 + X) + Y = 1 + (X + Y)
% 1 + X >> s(X)

% ?- add(s(zero), s(zero), T).
% s(X) = s(zero), Y = s(zero),  s(Z) = T , X = zero, add(zero, s(zero), Z)
% zero = zero, Y = s(zero), Z = Y, nat(s(zero)) -> true, T = s(Z) = s(Y) = s(s(zero)).
% answer T = s(s(zero)).

add(zero, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% abc_word(X) - X is word under alphabet {a, b, c}, epsilon, f(a, epsilon), f(a, f(b, epsilon))
% abc_word(epsilon).
%abc_word(X) :- X = starts_with(L, W), L = a, abc_word(W).
%abc_word(X) :- X = starts_with(L, W), L = b, abc_word(W).
%abc_word(X) :- X = starts_with(L, W), L = c, abc_word(W).

word(epsilon).
% starts_with(L, W) -> L.W
word(starts_with(L, W)) :- word(W), letter(L).
letter(a).
letter(b).
letter(c).
letter(d).

% member(X, W) -> X is letter of W
% member(X, W) :- word(W), W = starts_with(L, R), X = L.
member(L, starts_with(L, R)) :- letter(L), word(R). % :- word(starts_with(L, R))
% member(X, W) :- word(W), W = starts_with(L, R), member(X, R).
member(X, starts_with(L, W)) :- member(X, W).