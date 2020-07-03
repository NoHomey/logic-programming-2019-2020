% [1, 2, 3]
% [[1, 2], 3] - OK, but it could be simple

% [1, 2, 3, 4]
% ((1 - 2) - (3 - 4))
% sub(sub(num(1), num(2)), sum(num(3), num(4)))

% genTerm([X], num(X)).
% genTerm(L, sub(Left, Right)) :-
%     hasAtleastTwoElements(L),
%     split(L, Prefix, Sufix),
%     genTerm(Prefix, Left),
%     genTerm(Sufix, Right).

% hasAtleastTwoElements([_, _ | _]).

% split(L, P, S) :-
%     append(P, S, L),
%     P \= [],
%     S \= [].

% append([], L, L).
% append([H | T], L, [H | R]) :- append(T, L, R).

% evalTerm(num(X), X).
% evalTerm(num(X), Y) :- Y is - X.
% evalTerm(sub(Left, Right), Value) :-
%     evalTerm(Left, LeftValue),
%     evalTerm(Right, RightValue),
%     Value is LeftValue - RightValue.

% % canEval(L, Z)

% canEval([], 0). % a - b -> a + (-b)
% canEval(L, Z) :- genTerm(L, T), evalTerm(T, Z).

% (a - b) - c =?= a - (b - c)
% a = 1, b = 2, c = 3
% -1 - 3 = -4, 1 - (-1) = 2


canEval([], 0).
canEval([X], X).
canEval([X], Y) :- Y is -X.
canEval(L, Z) :-
    hasAtleastTwoElements(L),
    split(L, P, S),
    canEval(P, Left),
    canEval(S, Right),
    Z is Left - Right.

hasAtleastTwoElements([_, _ | _]).

split(L, P, S) :-
    append(P, S, L),
    P \= [],
    S \= [].

% (a - (b  - (c - d)))
% a - b + c - d

% genVectors(N, K, A, B)

% genVector(N, K, L) -> L is N element list with elements in the range [-K, K]
genVector(0, _, []).
genVector(N, K, [H | T]) :-
    N > 0,
    MK is -K,
    between(MK, K, H),
    N1 is N - 1,
    genVector(N1, K, T).

between(A, B, A) :- A =< B.
between(A, B, X) :- A < B, A1 is A + 1, between(A1, B, X).

dotProduct([], [], 0).
dotProduct([HA | TA], [HB | TB], S) :-
    dotProduct(TA, TB, R),
    S is R + HA * HB.

sumOfSquares(A, S) :- dotProduct(A, A, S).

productOfAbs([], 1).
productOfAbs([H | T], P) :-
    productOfAbs(T, R),
    abs(H, A),
    P is R * A.

abs(X, X) :- X >= 0.
abs(X, Y) :- X < 0, Y is -X.

genCandidates(N, K, A, B, V) :-
    genVector(N, K, A),
    genVector(N, K, B),
    dotProduct(A, B, 0),
    sumOfSquares(A, S),
    productOfAbs(B, P),
    V is P * S.

genVectors(N, K, A, B) :-
    genCandidates(N, K, A, B, V),
    not((
        genCandidates(N, K, _, _, U),
        U > V    
    )).

% listOfBetweens(A, B, L)
listOfBetweens(A, B, L) :-
    listOfBetweens_helper(A, B, L, []).

% (forall X : between(A, B))(X is membfer of Builder)
listOfBetweens_helper(A, B, Builder, Builder) :-
    not((
        between(A, B, X),
        not(member(X, Builder))
    )).

listOfBetweens_helper(A, B, L, Builder) :-
    between(A, B, X),
    not(member(X, Builder)),
    listOfBetweens_helper(A, B, L, [X | Builder]).