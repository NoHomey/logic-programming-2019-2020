min(A, A, B) :- A =< B.
% min(X0, X0, X1) :- X0 =< X1 -> min(X00, X00, X11) :- X00 =< X11
min(B, A, B) :- A > B.

min(M, [M]).
min(M, [H | T]) :- min(K, T), min(M, H, K).

nat(0).
nat(N) :- nat(K), N is K + 1.

even_member(X, L) :- member(X, L), X mod 2 =:= 0.
member(H, [H | _]).
member(X, [_ | T]) :- member(X, T).