female(juliet).
female(mary).
female(lisa).
female(sandra).

male(bob).
male(peter).
male(paul).
male(cris).
male(harry).
male(john).

parent(bob, lisa).
parent(bob, paul).
parent(bob, mary).
parent(juliet, lisa).
parent(juliet, paul).
parent(juliet, mary).

parent(peter, harry).
parent(lisa, harry).

parent(mary, john).
parent(mary, sandra).
parent(cris, john).
parent(cris, sandra).

% mother(X,Y) X is mother of Y
mother(X, Y) :- parent(X, Y), female(X).

% brother(X, Y) X is brother of Y
brother(X, Y) :- male(X), mother(Z, X), mother(Z, Y), X \= Y.

grandparent(G, C) :- parent(P, C), parent(G, P).

grandmother(G, C) :- grandparent(G, C), female(G).

sibling(X, Y) :- parent(P, X), parent(P, Y).

cousin(X, Y) :-
    grandmother(G, X),
    grandmother(G, Y),
    not((mother(Z, X), mother(Z, Y))).